{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hasura.RQL.DDL.Permission.Internal where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                        as M
import qualified Data.Text                                  as T
import qualified Database.PG.Query                          as Q
import qualified Hasura.Backends.Postgres.SQL.DML           as S

import           Control.Lens                               hiding ((.=))
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Text.Extended
import           Instances.TH.Lift                          ()
import           Language.Haskell.TH.Syntax                 (Lift)

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.Session


convColSpec :: FieldInfoMap (FieldInfo 'Postgres) -> PermColSpec -> [PGCol]
convColSpec _ (PCCols cols) = cols
convColSpec cim PCStar      = map pgiColumn $ getCols cim

permissionIsDefined
  :: Maybe (RolePermInfo backend) -> PermAccessor backend a -> Bool
permissionIsDefined rpi pa =
  isJust $ join $ rpi ^? _Just.permAccToLens pa

assertPermDefined
  :: (MonadError QErr m)
  => RoleName
  -> PermAccessor backend a
  -> TableInfo backend
  -> m ()
assertPermDefined roleName pa tableInfo =
  unless (permissionIsDefined rpi pa) $ throw400 PermissionDenied $ mconcat
  [ "'" <> T.pack (show $ permAccToType pa) <> "'"
  , " permission on " <>> _tciName (_tiCoreInfo tableInfo)
  , " for role " <>> roleName
  , " does not exist"
  ]
  where
    rpi = M.lookup roleName $ _tiRolePermInfoMap tableInfo

askPermInfo
  :: (MonadError QErr m)
  => TableInfo backend
  -> RoleName
  -> PermAccessor backend c
  -> m c
askPermInfo tabInfo roleName pa =
  case M.lookup roleName rpim >>= (^. paL) of
    Just c  -> return c
    Nothing -> throw400 PermissionDenied $ mconcat
               [ pt <> " permission on " <>> _tciName (_tiCoreInfo tabInfo)
               , " for role " <>> roleName
               , " does not exist"
               ]
  where
    paL = permAccToLens pa
    pt = permTypeToCode $ permAccToType pa
    rpim = _tiRolePermInfoMap tabInfo

savePermToCatalog
  :: (ToJSON a)
  => PermType
  -> QualifiedTable
  -> PermDef a
  -> SystemDefined
  -> Q.TxE QErr ()
savePermToCatalog pt (QualifiedObject sn tn) (PermDef  rn qdef mComment) systemDefined =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           INSERT INTO
               hdb_catalog.hdb_permission
               (table_schema, table_name, role_name, perm_type, perm_def, comment, is_system_defined)
           VALUES ($1, $2, $3, $4, $5 :: jsonb, $6, $7)
                |] (sn, tn, rn, permTypeToCode pt, Q.AltJ qdef, mComment, systemDefined) True

updatePermDefInCatalog
  :: (ToJSON a)
  => PermType
  -> QualifiedTable
  -> RoleName
  -> a
  -> Q.TxE QErr ()
updatePermDefInCatalog pt (QualifiedObject sn tn) rn qdef =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           UPDATE hdb_catalog.hdb_permission
              SET perm_def = $1 :: jsonb
             WHERE table_schema = $2 AND table_name = $3
               AND role_name = $4 AND perm_type = $5
           |] (Q.AltJ qdef, sn, tn, rn, permTypeToCode pt) True

dropPermFromCatalog
  :: QualifiedTable
  -> RoleName
  -> PermType
  -> Q.TxE QErr ()
dropPermFromCatalog (QualifiedObject sn tn) rn pt =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           DELETE FROM
               hdb_catalog.hdb_permission
           WHERE
               table_schema = $1
               AND table_name = $2
               AND role_name = $3
               AND perm_type = $4
                |] (sn, tn, rn, permTypeToCode pt) True

type CreatePerm a = WithTable (PermDef a)

data CreatePermP1Res a
  = CreatePermP1Res
  { cprInfo :: !a
  , cprDeps :: ![SchemaDependency]
  } deriving (Show, Eq)

procBoolExp
  :: (QErrM m, TableCoreInfoRM m)
  => QualifiedTable
  -> FieldInfoMap (FieldInfo 'Postgres)
  -> BoolExp 'Postgres
  -> m (AnnBoolExpPartialSQL 'Postgres, [SchemaDependency])
procBoolExp tn fieldInfoMap be = do
  abe <- annBoolExp valueParser fieldInfoMap $ unBoolExp be
  let deps = getBoolExpDeps tn abe
  return (abe, deps)

isReqUserId :: Text -> Bool
isReqUserId = (== "req_user_id") . T.toLower

getDepHeadersFromVal :: Value -> [Text]
getDepHeadersFromVal val = case val of
  Object o -> parseObject o
  _        -> parseOnlyString val
  where
    parseOnlyString v = case v of
      (String t)
        | isSessionVariable t -> [T.toLower t]
        | isReqUserId t -> [userIdHeader]
        | otherwise -> []
      _ -> []
    parseObject o =
      concatMap getDepHeadersFromVal (M.elems o)

getDependentHeaders :: BoolExp b -> [Text]
getDependentHeaders (BoolExp boolExp) =
  flip foldMap boolExp $ \(ColExp _ v) -> getDepHeadersFromVal v

valueParser
  :: (MonadError QErr m)
  => PGType (ColumnType 'Postgres) -> Value -> m (PartialSQLExp 'Postgres)
valueParser pgType = \case
  -- When it is a special variable
  String t
    | isSessionVariable t   -> return $ mkTypedSessionVar pgType $ mkSessionVariable t
    | isReqUserId t -> return $ mkTypedSessionVar pgType userIdHeader
  -- Typical value as Aeson's value
  val -> case pgType of
    PGTypeScalar columnType -> PSESQLExp . toTxtValue <$> parsePGScalarValue columnType val
    PGTypeArray ofType -> do
      vals <- runAesonParser parseJSON val
      WithScalarType scalarType scalarValues <- parsePGScalarValues ofType vals
      return . PSESQLExp $ S.SETyAnn
        (S.SEArray $ map (toTxtValue . WithScalarType scalarType) scalarValues)
        (S.mkTypeAnn $ PGTypeArray scalarType)

injectDefaults :: QualifiedTable -> QualifiedTable -> Q.Query
injectDefaults qv qt =
  Q.fromText $ mconcat
  [ "SELECT hdb_catalog.inject_table_defaults("
  , pgFmtLit vsn
  , ", "
  , pgFmtLit vn
  , ", "
  , pgFmtLit tsn
  , ", "
  , pgFmtLit tn
  , ");"
  ]

  where
    QualifiedObject (SchemaName vsn) (TableName vn) = qv
    QualifiedObject (SchemaName tsn) (TableName tn) = qt

data DropPerm a
  = DropPerm
  { dipTable :: !QualifiedTable
  , dipRole  :: !RoleName
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''DropPerm)

type family PermInfo a = r | r -> a
