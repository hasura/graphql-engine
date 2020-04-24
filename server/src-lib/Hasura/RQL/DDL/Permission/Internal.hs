{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hasura.RQL.DDL.Permission.Internal where

import           Control.Lens               hiding ((.=))
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.HashMap.Strict        as M
import qualified Data.Text.Extended         as T
import qualified Hasura.SQL.DML             as S

import           Hasura.EncJSON
import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Database.PG.Query          as Q

data PermColSpec
  = PCStar
  | PCCols ![PGCol]
  deriving (Show, Eq, Lift, Generic)
instance Cacheable PermColSpec

instance FromJSON PermColSpec where
  parseJSON (String "*") = return PCStar
  parseJSON x            = PCCols <$> parseJSON x

instance ToJSON PermColSpec where
  toJSON (PCCols cols) = toJSON cols
  toJSON PCStar        = "*"

convColSpec :: FieldInfoMap FieldInfo -> PermColSpec -> [PGCol]
convColSpec _ (PCCols cols) = cols
convColSpec cim PCStar      = map pgiColumn $ getCols cim

permissionIsDefined
  :: Maybe RolePermInfo -> PermAccessor a -> Bool
permissionIsDefined rpi pa =
  isJust $ join $ rpi ^? _Just.permAccToLens pa

assertPermDefined
  :: (MonadError QErr m)
  => RoleName
  -> PermAccessor a
  -> TableInfo
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
  => TableInfo
  -> RoleName
  -> PermAccessor c
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

data PermDef a =
  PermDef
  { pdRole       :: !RoleName
  , pdPermission :: !a
  , pdComment    :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift, Generic)
instance (Cacheable a) => Cacheable (PermDef a)
$(deriveFromJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''PermDef)

instance (ToJSON a) => ToJSON (PermDef a) where
  toJSON = object . toAesonPairs

instance (ToJSON a) => ToAesonPairs (PermDef a) where
 toAesonPairs (PermDef rn perm comment) =
  [ "role" .= rn
  , "permission" .= perm
  , "comment" .= comment
  ]

data CreatePermP1Res a
  = CreatePermP1Res
  { cprInfo :: !a
  , cprDeps :: ![SchemaDependency]
  } deriving (Show, Eq)

procBoolExp
  :: (QErrM m, TableCoreInfoRM m)
  => QualifiedTable
  -> FieldInfoMap FieldInfo
  -> BoolExp
  -> m (AnnBoolExpPartialSQL, [SchemaDependency])
procBoolExp tn fieldInfoMap be = do
  abe <- annBoolExp valueParser fieldInfoMap $ unBoolExp be
  let deps = getBoolExpDeps tn abe
  return (abe, deps)

isReqUserId :: T.Text -> Bool
isReqUserId = (== "req_user_id") . T.toLower

getDepHeadersFromVal :: Value -> [T.Text]
getDepHeadersFromVal val = case val of
  Object o -> parseObject o
  _        -> parseOnlyString val
  where
    parseOnlyString v = case v of
      (String t)
        | isUserVar t -> [T.toLower t]
        | isReqUserId t -> [userIdHeader]
        | otherwise -> []
      _ -> []
    parseObject o =
      concatMap getDepHeadersFromVal (M.elems o)

getDependentHeaders :: BoolExp -> [T.Text]
getDependentHeaders (BoolExp boolExp) =
  flip foldMap boolExp $ \(ColExp _ v) -> getDepHeadersFromVal v

valueParser
  :: (MonadError QErr m)
  => PGType PGColumnType -> Value -> m PartialSQLExp
valueParser pgType = \case
  -- When it is a special variable
  String t
    | isUserVar t   -> return $ mkTypedSessionVar pgType t
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

data DropPerm a
  = DropPerm
  { dipTable :: !QualifiedTable
  , dipRole  :: !RoleName
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''DropPerm)

type family PermInfo a = r | r -> a

class (ToJSON a) => IsPerm a where

  permAccessor
    :: PermAccessor (PermInfo a)

  buildPermInfo
    :: (QErrM m, TableCoreInfoRM m)
    => QualifiedTable
    -> FieldInfoMap FieldInfo
    -> PermDef a
    -> m (WithDeps (PermInfo a))

  getPermAcc1
    :: PermDef a -> PermAccessor (PermInfo a)
  getPermAcc1 _ = permAccessor

  getPermAcc2
    :: DropPerm a -> PermAccessor (PermInfo a)
  getPermAcc2 _ = permAccessor

addPermP2 :: (IsPerm a, MonadTx m, HasSystemDefined m) => QualifiedTable -> PermDef a -> m ()
addPermP2 tn pd = do
  let pt = permAccToType $ getPermAcc1 pd
  systemDefined <- askSystemDefined
  liftTx $ savePermToCatalog pt tn pd systemDefined

runCreatePerm
  :: (UserInfoM m, CacheRWM m, IsPerm a, MonadTx m, HasSystemDefined m)
  => CreatePerm a -> m EncJSON
runCreatePerm (WithTable tn pd) = do
  addPermP2 tn pd
  let pt = permAccToType $ getPermAcc1 pd
  buildSchemaCacheFor $ MOTableObj tn (MTOPerm (pdRole pd) pt)
  pure successMsg

dropPermP1
  :: (QErrM m, CacheRM m, IsPerm a)
  => DropPerm a -> m (PermInfo a)
dropPermP1 dp@(DropPerm tn rn) = do
  tabInfo <- askTabInfo tn
  askPermInfo tabInfo rn $ getPermAcc2 dp

dropPermP2 :: forall a m. (MonadTx m, IsPerm a) => DropPerm a -> m ()
dropPermP2 dp@(DropPerm tn rn) = do
  liftTx $ dropPermFromCatalog tn rn pt
  where
    pa = getPermAcc2 dp
    pt = permAccToType pa

runDropPerm
  :: (IsPerm a, UserInfoM m, CacheRWM m, MonadTx m)
  => DropPerm a -> m EncJSON
runDropPerm defn = do
  dropPermP1 defn
  dropPermP2 defn
  withNewInconsistentObjsCheck buildSchemaCache
  return successMsg
