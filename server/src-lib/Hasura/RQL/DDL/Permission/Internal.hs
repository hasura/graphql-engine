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
import           Hasura.Prelude
import           Hasura.RQL.GBoolExp
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.SQL.Types

import qualified Database.PG.Query          as Q

data PermColSpec
  = PCStar
  | PCCols ![PGCol]
  deriving (Show, Eq, Lift)

instance FromJSON PermColSpec where
  parseJSON (String "*") = return PCStar
  parseJSON x            = PCCols <$> parseJSON x

instance ToJSON PermColSpec where
  toJSON (PCCols cols) = toJSON cols
  toJSON PCStar        = "*"

convColSpec :: FieldInfoMap -> PermColSpec -> [PGCol]
convColSpec _ (PCCols cols) = cols
convColSpec cim PCStar      = map pgiName $ getCols cim

assertPermNotDefined
  :: (MonadError QErr m)
  => RoleName
  -> PermAccessor a
  -> TableInfo
  -> m ()
assertPermNotDefined roleName pa tableInfo =
  when (permissionIsDefined rpi pa || roleName == adminRole)
  $ throw400 AlreadyExists $ mconcat
  [ "'" <> T.pack (show $ permAccToType pa) <> "'"
  , " permission on " <>> tiName tableInfo
  , " for role " <>> roleName
  , " already exists"
  ]
  where
    rpi = M.lookup roleName $ tiRolePermInfoMap tableInfo

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
  , " permission on " <>> tiName tableInfo
  , " for role " <>> roleName
  , " does not exist"
  ]
  where
    rpi = M.lookup roleName $ tiRolePermInfoMap tableInfo

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
               [ pt <> " permisison on " <>> tiName tabInfo
               , " for role " <>> roleName
               , " does not exist"
               ]
  where
    paL = permAccToLens pa
    pt = permTypeToCode $ permAccToType pa
    rpim = tiRolePermInfoMap tabInfo

savePermToCatalog
  :: (ToJSON a)
  => PermType
  -> QualifiedTable
  -> PermDef a
  -> Q.TxE QErr ()
savePermToCatalog pt (QualifiedObject sn tn) (PermDef  rn qdef mComment) =
  Q.unitQE defaultTxErrorHandler [Q.sql|
           INSERT INTO
               hdb_catalog.hdb_permission
               (table_schema, table_name, role_name, perm_type, perm_def, comment)
           VALUES ($1, $2, $3, $4, $5 :: jsonb, $6)
                |] (sn, tn, rn, permTypeToCode pt, Q.AltJ qdef, mComment) True

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
  } deriving (Show, Eq, Lift)

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
  :: (QErrM m, CacheRM m)
  => QualifiedTable -> FieldInfoMap -> BoolExp
  -> m (AnnBoolExpPartialSQL, [SchemaDependency])
procBoolExp tn fieldInfoMap be = do
  abe <- annBoolExp valueParser fieldInfoMap be
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
  => PgType -> Value -> m PartialSQLExp
valueParser pgType = \case
  -- When it is a special variable
  String t
    | isUserVar t   -> return $ PSESessVar pgType t
    | isReqUserId t -> return $ PSESessVar pgType userIdHeader
    | otherwise     -> return $ PSESQLExp $
                       S.SETyAnn (S.SELit t) $ S.mkTypeAnn pgType

  -- Typical value as Aeson's value
  val -> case pgType of
    PgTypeSimple columnType -> PSESQLExp <$> txtRHSBuilder columnType val
    PgTypeArray ofType -> do
      vals <- runAesonParser parseJSON val
      arrayExp <- S.SEArray <$> indexedForM vals (txtRHSBuilder ofType)
      return $ PSESQLExp $ S.SETyAnn arrayExp $ S.mkTypeAnn pgType

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

class (ToJSON a) => IsPerm a where

  type DropPermP1Res a

  permAccessor
    :: PermAccessor (PermInfo a)

  buildPermInfo
    :: (QErrM m, CacheRM m)
    => TableInfo
    -> PermDef a
    -> m (WithDeps (PermInfo a))

  addPermP2Setup
    :: (MonadTx m) => QualifiedTable -> PermDef a -> PermInfo a -> m ()

  buildDropPermP1Res
    :: (QErrM m, CacheRM m, UserInfoM m)
    => DropPerm a
    -> m (DropPermP1Res a)

  dropPermP2Setup
    :: (CacheRWM m, MonadTx m) => DropPerm a -> DropPermP1Res a -> m ()

  getPermAcc1
    :: PermDef a -> PermAccessor (PermInfo a)
  getPermAcc1 _ = permAccessor

  getPermAcc2
    :: DropPerm a -> PermAccessor (PermInfo a)
  getPermAcc2 _ = permAccessor

validateViewPerm
  :: (IsPerm a, QErrM m) => PermDef a -> TableInfo -> m ()
validateViewPerm permDef tableInfo =
  case permAcc of
    PASelect -> return ()
    PAInsert -> mutableView tn viIsInsertable viewInfo "insertable"
    PAUpdate -> mutableView tn viIsUpdatable viewInfo "updatable"
    PADelete -> mutableView tn viIsDeletable viewInfo "deletable"
  where
    tn = tiName tableInfo
    viewInfo = tiViewInfo tableInfo
    permAcc = getPermAcc1 permDef

addPermP1
  :: (QErrM m, CacheRM m, IsPerm a)
  => TableInfo -> PermDef a -> m (WithDeps (PermInfo a))
addPermP1 tabInfo pd = do
  assertPermNotDefined (pdRole pd) (getPermAcc1 pd) tabInfo
  buildPermInfo tabInfo pd

addPermP2 :: (IsPerm a, QErrM m, CacheRWM m, MonadTx m)
          => QualifiedTable -> PermDef a -> WithDeps (PermInfo a) -> m ()
addPermP2 tn pd (permInfo, deps) = do
  addPermP2Setup tn pd permInfo
  addPermToCache tn (pdRole pd) pa permInfo deps
  liftTx $ savePermToCatalog pt tn pd
  where
    pa = getPermAcc1 pd
    pt = permAccToType pa

createPermP1
  :: ( UserInfoM m, MonadError QErr m
     , CacheRM m, IsPerm a
     )
  => WithTable (PermDef a) -> m (WithDeps (PermInfo a))
createPermP1 (WithTable tn pd) = do
  adminOnly
  tabInfo <- askTabInfo tn
  validateViewPerm pd tabInfo
  addPermP1 tabInfo pd

runCreatePerm
  :: ( UserInfoM m
     , CacheRWM m, IsPerm a, MonadTx m
     )
  => CreatePerm a -> m EncJSON
runCreatePerm defn@(WithTable tn pd) = do
  permInfo <- createPermP1 defn
  addPermP2 tn pd permInfo
  return successMsg

dropPermP1
  :: (QErrM m, CacheRM m, UserInfoM m, IsPerm a)
  => DropPerm a -> m (PermInfo a)
dropPermP1 dp@(DropPerm tn rn) = do
  adminOnly
  tabInfo <- askTabInfo tn
  askPermInfo tabInfo rn $ getPermAcc2 dp

dropPermP2
  :: (IsPerm a, QErrM m, CacheRWM m, MonadTx m)
  => DropPerm a -> DropPermP1Res a -> m ()
dropPermP2 dp@(DropPerm tn rn) p1Res = do
  dropPermP2Setup dp p1Res
  delPermFromCache pa rn tn
  liftTx $ dropPermFromCatalog tn rn pt
  where
    pa = getPermAcc2 dp
    pt = permAccToType pa

runDropPerm
  :: (IsPerm a, UserInfoM m, CacheRWM m, MonadTx m)
  => DropPerm a -> m EncJSON
runDropPerm defn = do
  permInfo <- buildDropPermP1Res defn
  dropPermP2 defn permInfo
  return successMsg
