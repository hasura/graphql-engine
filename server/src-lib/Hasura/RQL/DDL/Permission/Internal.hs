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

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.Backends.Postgres.Translate.Column
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.Session
import           Hasura.SQL.Types


convColSpec :: FieldInfoMap (FieldInfo 'Postgres) -> PermColSpec -> [PGCol]
convColSpec _ (PCCols cols) = cols
convColSpec cim PCStar      = map pgiColumn $ getCols cim

permissionIsDefined
  :: Maybe (RolePermInfo backend) -> PermAccessor backend a -> Bool
permissionIsDefined rpi pa =
  isJust $ join $ rpi ^? _Just.permAccToLens pa

assertPermDefined
  :: (Backend backend, MonadError QErr m)
  => RoleName
  -> PermAccessor backend a
  -> TableInfo backend
  -> m ()
assertPermDefined roleName pa tableInfo =
  unless (permissionIsDefined rpi pa) $ throw400 PermissionDenied $ mconcat
  [ "'" <> tshow (permAccToType pa) <> "'"
  , " permission on " <>> _tciName (_tiCoreInfo tableInfo)
  , " for role " <>> roleName
  , " does not exist"
  ]
  where
    rpi = M.lookup roleName $ _tiRolePermInfoMap tableInfo

askPermInfo
  :: (Backend backend, MonadError QErr m)
  => TableInfo backend
  -> RoleName
  -> PermAccessor backend c
  -> m c
askPermInfo tabInfo roleName pa =
  (M.lookup roleName rpim >>= (^. permAccToLens pa))
  `onNothing`
  throw400 PermissionDenied
  (mconcat
    [ pt <> " permission on " <>> _tciName (_tiCoreInfo tabInfo)
    , " for role " <>> roleName
    , " does not exist"
    ])
  where
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
  :: (QErrM m, TableCoreInfoRM 'Postgres m)
  => SourceName
  -> QualifiedTable
  -> FieldInfoMap (FieldInfo 'Postgres)
  -> BoolExp 'Postgres
  -> m (AnnBoolExpPartialSQL 'Postgres, [SchemaDependency])
procBoolExp source tn fieldInfoMap be = do
  abe <- annBoolExp valueParser fieldInfoMap $ unBoolExp be
  let deps = getBoolExpDeps source tn abe
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
  => CollectableType (ColumnType 'Postgres) -> Value -> m (PartialSQLExp 'Postgres)
valueParser pgType = \case
  -- When it is a special variable
  String t
    | isSessionVariable t -> return $ mkTypedSessionVar pgType $ mkSessionVariable t
    | isReqUserId t       -> return $ mkTypedSessionVar pgType userIdHeader
  -- Typical value as Aeson's value
  val -> case pgType of
    CollectableTypeScalar cvType ->
      PSESQLExp . toTxtValue . ColumnValue cvType <$> parsePGScalarValue cvType val
    CollectableTypeArray ofType -> do
      vals <- runAesonParser parseJSON val
      scalarValues <- parsePGScalarValues ofType vals
      return . PSESQLExp $ S.SETyAnn
        (S.SEArray $ map (toTxtValue . ColumnValue ofType) scalarValues)
        (S.mkTypeAnn $ CollectableTypeArray (unsafePGColumnToBackend ofType))

mkTypedSessionVar :: CollectableType (ColumnType 'Postgres) -> SessionVariable -> PartialSQLExp 'Postgres
mkTypedSessionVar columnType =
  PSESessVar (unsafePGColumnToBackend <$> columnType)

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
  { dipSource :: !SourceName
  , dipTable  :: !QualifiedTable
  , dipRole   :: !RoleName
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''DropPerm)

instance FromJSON (DropPerm a) where
  parseJSON = withObject "DropPerm" $ \o ->
    DropPerm
    <$> o .:? "source" .!= defaultSource
    <*> o .: "table"
    <*> o .: "role"

type family PermInfo a = r | r -> a
