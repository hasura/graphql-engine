{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.DDL.Permission.Internal
  ( CreatePerm (..),
    DropPerm (..),
    permissionIsDefined,
    assertPermDefined,
    interpColSpec,
    getDepHeadersFromVal,
    getDependentHeaders,
    annBoolExp,
    procBoolExp,
    procLogicalModelBoolExp,
  )
where

import Control.Lens hiding ((.=))
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as Set
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.LogicalModel.Types (LogicalModelName)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BoolExp
import Hasura.RQL.Types.Column (ColumnReference (ColumnReferenceColumn))
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Relationships.Local
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Table
import Hasura.Server.Utils

-- | Intrepet a 'PermColSpec' column specification, which can either refer to a
-- list of named columns or all columns.
interpColSpec :: [Column b] -> PermColSpec b -> [Column b]
interpColSpec _ (PCCols cols) = cols
interpColSpec allColumns PCStar = allColumns

permissionIsDefined ::
  PermType -> RolePermInfo backend -> Bool
permissionIsDefined pt rpi = isJust
  case pt of
    PTSelect -> rpi ^. permSel $> ()
    PTInsert -> rpi ^. permIns $> ()
    PTUpdate -> rpi ^. permUpd $> ()
    PTDelete -> rpi ^. permDel $> ()

assertPermDefined ::
  (Backend backend, MonadError QErr m) =>
  RoleName ->
  PermType ->
  TableInfo backend ->
  m ()
assertPermDefined role pt tableInfo =
  unless (any (permissionIsDefined pt) rpi) $
    throw400 PermissionDenied $
      "'"
        <> tshow pt
        <> "'"
        <> " permission on "
        <> tableInfoName tableInfo
          <<> " for role "
        <> role
          <<> " does not exist"
  where
    rpi = HashMap.lookup role $ _tiRolePermInfoMap tableInfo

newtype CreatePerm a b = CreatePerm (WithTable b (PermDef b a))

deriving instance (Backend b, FromJSON (PermDef b a)) => FromJSON (CreatePerm a b)

data CreatePermP1Res a = CreatePermP1Res
  { cprInfo :: a,
    cprDeps :: [SchemaDependency]
  }
  deriving (Show, Eq)

procBoolExp ::
  ( QErrM m,
    TableCoreInfoRM b m,
    BackendMetadata b,
    GetAggregationPredicatesDeps b
  ) =>
  SourceName ->
  TableName b ->
  FieldInfoMap (FieldInfo b) ->
  BoolExp b ->
  m (AnnBoolExpPartialSQL b, Seq SchemaDependency)
procBoolExp source tn fieldInfoMap be = do
  let rhsParser = BoolExpRHSParser parseCollectableType PSESession

  rootFieldInfoMap <-
    fmap _tciFieldInfoMap $
      lookupTableCoreInfo tn
        `onNothingM` throw500 ("unexpected: " <> tn <<> " doesn't exist")

  abe <- annBoolExp rhsParser rootFieldInfoMap fieldInfoMap $ unBoolExp be
  let deps = getBoolExpDeps source tn abe
  return (abe, Seq.fromList deps)

-- | Interpret a 'BoolExp' into an 'AnnBoolExp', collecting any dependencies as
-- we go. At the moment, the only dependencies we're likely to encounter are
-- independent dependencies on other tables. For example, "this user can only
-- select from this logical model if their ID is in the @allowed_users@ table".
procLogicalModelBoolExp ::
  forall b m.
  ( QErrM m,
    TableCoreInfoRM b m,
    BackendMetadata b,
    GetAggregationPredicatesDeps b
  ) =>
  SourceName ->
  LogicalModelName ->
  FieldInfoMap (FieldInfo b) ->
  BoolExp b ->
  m (AnnBoolExpPartialSQL b, Seq SchemaDependency)
procLogicalModelBoolExp source lmn fieldInfoMap be = do
  let -- The parser for the "right hand side" of operations. We use @rhsParser@
      -- as the name here for ease of grepping, though it's maybe a bit vague.
      -- More specifically, if we think of an operation that combines a field
      -- (such as those in tables or native queries) on the /left/ with a value
      -- or session variable on the /right/, this is a parser for the latter.
      rhsParser :: BoolExpRHSParser b m (PartialSQLExp b)
      rhsParser = BoolExpRHSParser parseCollectableType PSESession

  -- In Native Queries, there are no relationships (unlike tables, where one
  -- table can reference another). This means that our root fieldInfoMap is
  -- always going to be the same as our current fieldInfoMap, so we just pass
  -- the same one in twice.
  abe <- annBoolExp rhsParser fieldInfoMap fieldInfoMap (unBoolExp be)

  let -- What dependencies do we have on the schema cache in order to process
      -- this boolean expression? This dependency system is explained more
      -- thoroughly in the 'buildLogicalModelSelPermInfo' inline comments.
      deps :: [SchemaDependency]
      deps = getLogicalModelBoolExpDeps source lmn abe

  return (abe, Seq.fromList deps)

annBoolExp ::
  (QErrM m, TableCoreInfoRM b m, BackendMetadata b) =>
  BoolExpRHSParser b m v ->
  FieldInfoMap (FieldInfo b) ->
  FieldInfoMap (FieldInfo b) ->
  GBoolExp b ColExp ->
  m (AnnBoolExp b v)
annBoolExp rhsParser rootFieldInfoMap fim boolExp =
  case boolExp of
    BoolAnd exps -> BoolAnd <$> procExps exps
    BoolOr exps -> BoolOr <$> procExps exps
    BoolNot e -> BoolNot <$> annBoolExp rhsParser rootFieldInfoMap fim e
    BoolExists (GExists refqt whereExp) ->
      withPathK "_exists" $ do
        refFields <- withPathK "_table" $ askFieldInfoMapSource refqt
        annWhereExp <- withPathK "_where" $ annBoolExp rhsParser rootFieldInfoMap refFields whereExp
        return $ BoolExists $ GExists refqt annWhereExp
    BoolField fld -> BoolField <$> annColExp rhsParser rootFieldInfoMap fim fld
  where
    procExps = mapM (annBoolExp rhsParser rootFieldInfoMap fim)

annColExp ::
  (QErrM m, TableCoreInfoRM b m, BackendMetadata b) =>
  BoolExpRHSParser b m v ->
  FieldInfoMap (FieldInfo b) ->
  FieldInfoMap (FieldInfo b) ->
  ColExp ->
  m (AnnBoolExpFld b v)
annColExp rhsParser rootFieldInfoMap colInfoMap (ColExp fieldName colVal) = do
  colInfo <- askFieldInfo colInfoMap fieldName
  case colInfo of
    FIColumn pgi -> AVColumn pgi <$> parseBoolExpOperations (_berpValueParser rhsParser) rootFieldInfoMap colInfoMap (ColumnReferenceColumn pgi) colVal
    FINestedObject {} ->
      throw400 NotSupported "nested object not supported"
    FIRelationship relInfo -> do
      relBoolExp <- decodeValue colVal
      relFieldInfoMap <- askFieldInfoMapSource $ riRTable relInfo
      annRelBoolExp <- annBoolExp rhsParser rootFieldInfoMap relFieldInfoMap $ unBoolExp relBoolExp
      return $
        AVRelationship
          relInfo
          ( RelationshipFilters
              { -- Note that we do not include the permissions of the target table, since
                -- those only apply to GraphQL queries.
                rfTargetTablePermissions = BoolAnd [],
                rfFilter = annRelBoolExp
              }
          )
    FIComputedField computedFieldInfo ->
      AVComputedField <$> buildComputedFieldBooleanExp (BoolExpResolver annBoolExp) rhsParser rootFieldInfoMap colInfoMap computedFieldInfo colVal
    -- Using remote fields in the boolean expression is not supported.
    FIRemoteRelationship {} ->
      throw400 UnexpectedPayload "remote field unsupported"

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
      concatMap getDepHeadersFromVal (KM.elems o)

getDependentHeaders :: BoolExp b -> HashSet Text
getDependentHeaders (BoolExp boolExp) =
  Set.fromList $ flip foldMap boolExp $ \(ColExp _ v) -> getDepHeadersFromVal v

data DropPerm b = DropPerm
  { dipSource :: SourceName,
    dipTable :: TableName b,
    dipRole :: RoleName
  }

instance (Backend b) => FromJSON (DropPerm b) where
  parseJSON = withObject "DropPerm" $ \o ->
    DropPerm
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "role"
