module Hasura.RQL.DDL.Permission.Internal where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                        as M
import qualified Data.Text                                  as T

import           Control.Lens                               hiding ((.=))
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Kind                                  (Type)
import           Data.Text.Extended

import           Hasura.Backends.Postgres.Translate.BoolExp
import           Hasura.Base.Error
import           Hasura.RQL.Types
import           Hasura.Server.Utils
import           Hasura.Session


convColSpec :: FieldInfoMap (FieldInfo b) -> PermColSpec b -> [Column b]
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
assertPermDefined role pa tableInfo =
  unless (permissionIsDefined rpi pa) $ throw400 PermissionDenied $ mconcat
  [ "'" <> tshow (permAccToType pa) <> "'"
  , " permission on " <>> tableInfoName tableInfo
  , " for role " <>> role
  , " does not exist"
  ]
  where
    rpi = M.lookup role $ _tiRolePermInfoMap tableInfo

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
    [ pt <> " permission on " <>> tableInfoName tabInfo
    , " for role " <>> roleName
    , " does not exist"
    ])
  where
    pt = permTypeToCode $ permAccToType pa
    rpim = _tiRolePermInfoMap tabInfo


newtype CreatePerm a b = CreatePerm (WithTable b (PermDef (a b)))
  deriving newtype (Show, Eq, FromJSON, ToJSON)

data CreatePermP1Res a
  = CreatePermP1Res
  { cprInfo :: !a
  , cprDeps :: ![SchemaDependency]
  } deriving (Show, Eq)

procBoolExp
  :: (QErrM m, TableCoreInfoRM b m, BackendMetadata b)
  => SourceName
  -> TableName b
  -> FieldInfoMap (FieldInfo b)
  -> BoolExp b
  -> m (AnnBoolExpPartialSQL b, [SchemaDependency])
procBoolExp source tn fieldInfoMap be = do
  abe <- annBoolExp parseCollectableType tn fieldInfoMap $ unBoolExp be
  let deps = getBoolExpDeps source tn abe
  return (abe, deps)

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

data DropPerm (a :: BackendType -> Type) b
  = DropPerm
  { dipSource :: !SourceName
  , dipTable  :: !(TableName b)
  , dipRole   :: !RoleName
  } deriving (Generic)
deriving instance (Backend b) => Show (DropPerm a b)
deriving instance (Backend b) => Eq   (DropPerm a b)
instance (Backend b) => ToJSON (DropPerm a b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields=True}

instance (Backend b) => FromJSON (DropPerm a b) where
  parseJSON = withObject "DropPerm" $ \o ->
    DropPerm
    <$> o .:? "source" .!= defaultSource
    <*> o .: "table"
    <*> o .: "role"
