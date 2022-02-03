module Hasura.RQL.DDL.Permission.Internal
  ( CreatePerm (..),
    DropPerm (..),
    assertPermDefined,
    interpColSpec,
    getDepHeadersFromVal,
    getDependentHeaders,
    procBoolExp,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson.Types
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as Set
import Data.Kind (Type)
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Backends.Postgres.Translate.BoolExp
import Hasura.Base.Error
import Hasura.Prelude
import Hasura.RQL.Types
import Hasura.Server.Utils
import Hasura.Session

-- | Intrepet a 'PermColSpec' column specification, which can either refer to a
-- list of named columns or all columns.
interpColSpec :: [Column b] -> PermColSpec b -> [Column b]
interpColSpec _ (PCCols cols) = cols
interpColSpec allColumns PCStar = allColumns

permissionIsDefined ::
  Maybe (RolePermInfo backend) -> PermAccessor backend a -> Bool
permissionIsDefined rpi pa =
  isJust $ join $ rpi ^? _Just . permAccToLens pa

assertPermDefined ::
  (Backend backend, MonadError QErr m) =>
  RoleName ->
  PermAccessor backend a ->
  TableInfo backend ->
  m ()
assertPermDefined role pa tableInfo =
  unless (permissionIsDefined rpi pa) $
    throw400 PermissionDenied $
      mconcat
        [ "'" <> tshow (permAccToType pa) <> "'",
          " permission on " <>> tableInfoName tableInfo,
          " for role " <>> role,
          " does not exist"
        ]
  where
    rpi = M.lookup role $ _tiRolePermInfoMap tableInfo

newtype CreatePerm a b = CreatePerm (WithTable b (PermDef (a b)))
  deriving newtype (FromJSON)

data CreatePermP1Res a = CreatePermP1Res
  { cprInfo :: !a,
    cprDeps :: ![SchemaDependency]
  }
  deriving (Show, Eq)

procBoolExp ::
  (QErrM m, TableCoreInfoRM b m, BackendMetadata b) =>
  SourceName ->
  TableName b ->
  FieldInfoMap (FieldInfo b) ->
  BoolExp b ->
  m (AnnBoolExpPartialSQL b, [SchemaDependency])
procBoolExp source tn fieldInfoMap be = do
  let rhsParser = BoolExpRHSParser parseCollectableType PSESession
  abe <- annBoolExp rhsParser tn fieldInfoMap $ unBoolExp be
  let deps = getBoolExpDeps source tn abe
  return (abe, deps)

getDepHeadersFromVal :: Value -> [Text]
getDepHeadersFromVal val = case val of
  Object o -> parseObject o
  _ -> parseOnlyString val
  where
    parseOnlyString v = case v of
      (String t)
        | isSessionVariable t -> [T.toLower t]
        | isReqUserId t -> [userIdHeader]
        | otherwise -> []
      _ -> []
    parseObject o =
      concatMap getDepHeadersFromVal (M.elems o)

getDependentHeaders :: BoolExp b -> HashSet Text
getDependentHeaders (BoolExp boolExp) =
  Set.fromList $ flip foldMap boolExp $ \(ColExp _ v) -> getDepHeadersFromVal v

data DropPerm (a :: BackendType -> Type) b = DropPerm
  { dipSource :: !SourceName,
    dipTable :: !(TableName b),
    dipRole :: !RoleName
  }

instance (Backend b) => FromJSON (DropPerm a b) where
  parseJSON = withObject "DropPerm" $ \o ->
    DropPerm
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "role"
