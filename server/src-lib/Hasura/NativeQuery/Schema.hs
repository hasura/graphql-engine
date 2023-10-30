{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Schema parsers for native queries.
module Hasura.NativeQuery.Schema
  ( defaultSelectNativeQuery,
    defaultSelectNativeQueryObject,
    defaultBuildNativeQueryRootFields,
  )
where

import Data.Has (Has (getter))
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Set qualified as S
import Hasura.GraphQL.Schema.Backend
  ( BackendLogicalModelSelectSchema (..),
    BackendNativeQuerySelectSchema (..),
    BackendTableSelectSchema (..),
    MonadBuildSchema,
    tableSelectionSet,
  )
import Hasura.GraphQL.Schema.Common
  ( AnnotatedField,
    AnnotatedFields,
    SchemaT,
    askNativeQueryInfo,
    askTableInfo,
    parsedSelectionsToFields,
    retrieve,
    scRole,
    tablePermissionsInfo,
    textToName,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Table (tableSelectPermissions)
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.Schema (buildLogicalModelIR, buildLogicalModelPermissions, logicalModelFieldParsers, logicalModelSelectionList)
import Hasura.LogicalModelResolver.Schema (argumentsSchema)
import Hasura.NativeQuery.Cache (NativeQueryInfo (..))
import Hasura.NativeQuery.IR (NativeQuery (..))
import Hasura.NativeQuery.Metadata (ArgumentName (..), InterpolatedQuery (..))
import Hasura.NativeQuery.Types (NullableScalarType (..), getNativeQueryName)
import Hasura.Prelude
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Select (QueryDB (QDBMultipleRows))
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value (Provenance (FreshVar), UnpreparedValue (UVParameter))
import Hasura.RQL.Types.Column qualified as Column
import Hasura.RQL.Types.Common (RelType (..), relNameToTxt)
import Hasura.RQL.Types.Metadata.Object qualified as MO
import Hasura.RQL.Types.Relationships.Local (Nullable (..), RelInfo (..), RelMapping (..), RelTarget (..))
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.Source
  ( SourceInfo (_siCustomization, _siName),
  )
import Hasura.RQL.Types.SourceCustomization
  ( ResolvedSourceCustomization (_rscNamingConvention),
  )
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Language.GraphQL.Draft.Syntax qualified as G

defaultSelectNativeQueryObject ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendNativeQuerySelectSchema b,
    BackendTableSelectSchema b
  ) =>
  -- native query info
  NativeQueryInfo b ->
  -- field name
  G.Name ->
  -- field description, if any
  Maybe G.Description ->
  SchemaT
    r
    m
    (Maybe (P.FieldParser n (IR.AnnObjectSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
defaultSelectNativeQueryObject nqi@NativeQueryInfo {..} fieldName description = runMaybeT $ do
  nativeQueryArgsParser <-
    nativeQueryArgumentsSchema @b @r @m @n fieldName _nqiArguments

  sourceInfo :: SourceInfo b <- asks getter

  let sourceName = _siName sourceInfo

  logicalModelPermissions <-
    MaybeT
      $ buildLogicalModelPermissions @b @r @m @n _nqiReturns

  -- if we have any relationships, we use a Native Query rather than Logical
  -- Model parser
  let hasExtraFields = not (null _nqiRelationships)

  selectionSetParser <-
    if hasExtraFields
      then MaybeT $ nativeQuerySelectionSet nqi
      else MaybeT $ logicalModelSelectionSet _nqiReturns

  let sourceObj =
        MO.MOSourceObjId
          sourceName
          (mkAnyBackend $ MO.SMONativeQuery @b _nqiRootFieldName)

  lift $ P.memoizeOn 'defaultSelectNativeQueryObject (sourceName, fieldName) do
    pure
      $ P.setFieldParserOrigin sourceObj
      $ P.subselection
        fieldName
        description
        nativeQueryArgsParser
        selectionSetParser
      <&> \(nqArgs, fields) ->
        IR.AnnObjectSelectG
          fields
          ( IR.FromNativeQuery
              NativeQuery
                { nqRootFieldName = _nqiRootFieldName,
                  nqInterpolatedQuery = interpolatedQuery _nqiCode nqArgs,
                  nqLogicalModel = buildLogicalModelIR _nqiReturns
                }
          )
          (IR._tpFilter logicalModelPermissions)

nativeQuerySelectionList ::
  (MonadBuildSchema b r m n, BackendNativeQuerySelectSchema b, BackendTableSelectSchema b) =>
  Nullable ->
  NativeQueryInfo b ->
  SchemaT r m (Maybe (P.Parser 'P.Output n (AnnotatedFields b)))
nativeQuerySelectionList nullability nativeQuery =
  fmap nullabilityModifier <$> nativeQuerySelectionSet nativeQuery
  where
    nullabilityModifier =
      case nullability of
        Nullable -> nullableObjectList
        NotNullable -> nonNullableObjectList

    -- \| Converts an output type parser from object_type to [object_type!]!
    nonNullableObjectList :: P.Parser 'P.Output m a -> P.Parser 'P.Output m a
    nonNullableObjectList =
      P.nonNullableParser . P.multiple . P.nonNullableParser

    -- \| Converts an output type parser from object_type to [object_type!]
    nullableObjectList :: P.Parser 'P.Output m a -> P.Parser 'P.Output m a
    nullableObjectList =
      P.multiple . P.nonNullableParser

-- | select a native query - implementation is the same for root fields and
-- array relationships
defaultSelectNativeQuery ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendNativeQuerySelectSchema b,
    BackendTableSelectSchema b
  ) =>
  -- native query info
  NativeQueryInfo b ->
  -- field name
  G.Name ->
  -- are fields nullable?
  Nullable ->
  -- field description, if any
  Maybe G.Description ->
  SchemaT
    r
    m
    (Maybe (P.FieldParser n (IR.AnnSimpleSelectG b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
defaultSelectNativeQuery nqi@NativeQueryInfo {..} fieldName nullability description = runMaybeT $ do
  nativeQueryArgsParser <-
    nativeQueryArgumentsSchema @b @r @m @n fieldName _nqiArguments

  sourceInfo :: SourceInfo b <- asks getter

  let sourceName = _siName sourceInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo

  stringifyNumbers <- retrieve Options.soStringifyNumbers

  logicalModelPermissions <-
    MaybeT
      $ buildLogicalModelPermissions @b @r @m @n _nqiReturns

  -- if we have any relationships, we use a Native Query rather than Logical
  -- Model parser
  let hasExtraFields = not (null _nqiRelationships)

  selectionListParser <-
    if hasExtraFields
      then MaybeT $ nativeQuerySelectionList nullability nqi
      else MaybeT $ logicalModelSelectionList nullability _nqiReturns

  logicalModelsArgsParser <- lift $ logicalModelArguments @b @r @m @n _nqiReturns

  let sourceObj =
        MO.MOSourceObjId
          sourceName
          (mkAnyBackend $ MO.SMONativeQuery @b _nqiRootFieldName)

  pure
    $ P.setFieldParserOrigin sourceObj
    $ P.subselection
      fieldName
      description
      ( (,)
          <$> logicalModelsArgsParser
          <*> nativeQueryArgsParser
      )
      selectionListParser
    <&> \((lmArgs, nqArgs), fields) ->
      IR.AnnSelectG
        { IR._asnFields = fields,
          IR._asnFrom =
            IR.FromNativeQuery
              NativeQuery
                { nqRootFieldName = _nqiRootFieldName,
                  nqInterpolatedQuery = interpolatedQuery _nqiCode nqArgs,
                  nqLogicalModel = buildLogicalModelIR _nqiReturns
                },
          IR._asnPerm = logicalModelPermissions,
          IR._asnArgs = lmArgs,
          IR._asnStrfyNum = stringifyNumbers,
          IR._asnNamingConvention = Just tCase
        }

defaultBuildNativeQueryRootFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendNativeQuerySelectSchema b,
    BackendTableSelectSchema b
  ) =>
  NativeQueryInfo b ->
  SchemaT
    r
    m
    (Maybe (P.FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
defaultBuildNativeQueryRootFields nqi@NativeQueryInfo {..} = do
  let fieldName = getNativeQueryName _nqiRootFieldName
      description = G.Description <$> _nqiDescription
  (fmap . fmap . fmap) QDBMultipleRows (defaultSelectNativeQuery nqi fieldName NotNullable description)

nativeQueryArgumentsSchema ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  G.Name ->
  HashMap ArgumentName (NullableScalarType b) ->
  MaybeT (SchemaT r m) (P.InputFieldsParser n (HashMap ArgumentName (Column.ColumnValue b)))
nativeQueryArgumentsSchema = argumentsSchema "Native Query"

-- | swap the template names in our query for unprepared values
interpolatedQuery ::
  InterpolatedQuery ArgumentName ->
  HashMap ArgumentName (Column.ColumnValue b) ->
  InterpolatedQuery (UnpreparedValue b)
interpolatedQuery nqiCode nqArgs =
  InterpolatedQuery
    $ (fmap . fmap)
      ( \var -> case HashMap.lookup var nqArgs of
          Just arg -> UVParameter FreshVar arg
          Nothing ->
            -- the `nativeQueryArgsParser` will already have checked
            -- we have all the args the query needs so this _should
            -- not_ happen
            error $ "No native query arg passed for " <> show var
      )
      (getInterpolatedQuery nqiCode)

-- these functions become specific to the suppliers of the types
-- again, as they must
-- a) get the field parsers for the Logical Model
-- b) add any parsers for relationships etc
nativeQuerySelectionSet ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendNativeQuerySelectSchema b,
    BackendTableSelectSchema b
  ) =>
  NativeQueryInfo b ->
  SchemaT r m (Maybe (P.Parser 'P.Output n (AnnotatedFields b)))
nativeQuerySelectionSet nativeQuery = runMaybeT do
  let logicalModel = _nqiReturns nativeQuery
      description = G.Description <$> _lmiDescription logicalModel

  -- what name shall we call the selection set? (and thus, it's type in GraphQL
  -- schema?)
  let typeName = getNativeQueryName (_nqiRootFieldName nativeQuery)

  -- What interfaces does this type implement?
  let implementsInterfaces = []

  lift $ P.memoizeOn 'nativeQuerySelectionSet typeName do
    -- list of relationship names to allow as Logimo fields
    let knownRelNames = S.fromList $ InsOrdHashMap.keys $ _nqiRelationships nativeQuery

    -- a pile 'o' parsers
    logicalModelFields <- logicalModelFieldParsers knownRelNames logicalModel

    relationshipFields <- catMaybes <$> traverse nativeQueryRelationshipField (InsOrdHashMap.elems $ _nqiRelationships nativeQuery)

    let parsers = relationshipFields <> logicalModelFields

    pure
      $ P.selectionSetObject typeName description parsers implementsInterfaces
      <&> parsedSelectionsToFields IR.AFExpression

-- | Field parsers for a logical model object relationship
nativeQueryRelationshipField ::
  forall b r m n.
  ( BackendNativeQuerySelectSchema b,
    BackendTableSelectSchema b,
    MonadBuildSchema b r m n
  ) =>
  RelInfo b ->
  SchemaT r m (Maybe (P.FieldParser n (AnnotatedField b)))
nativeQueryRelationshipField ri | riType ri == ObjRel = runMaybeT do
  relFieldName <- lift $ textToName $ relNameToTxt $ riName ri

  case riTarget ri of
    RelTargetNativeQuery nativeQueryName -> do
      nativeQueryInfo <- askNativeQueryInfo nativeQueryName

      let objectRelDesc = Just $ G.Description "An object relationship"

      nativeQueryParser <-
        MaybeT $ selectNativeQueryObject nativeQueryInfo relFieldName objectRelDesc

      -- this only affects the generated GraphQL type
      let nullability = Nullable

      pure
        $ nativeQueryParser
        <&> \selectExp ->
          IR.AFObjectRelation (IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) nullability selectExp)
    RelTargetTable otherTableName -> do
      let desc = Just $ G.Description "An object relationship"
      roleName <- retrieve scRole
      otherTableInfo <- lift $ askTableInfo otherTableName
      remotePerms <- hoistMaybe $ tableSelectPermissions roleName otherTableInfo
      selectionSetParser <- MaybeT $ tableSelectionSet otherTableInfo
      pure
        $ P.subselection_ relFieldName desc selectionSetParser
        <&> \fields ->
          IR.AFObjectRelation
            $ IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) Nullable
            $ IR.AnnObjectSelectG fields (IR.FromTable otherTableName)
            $ IR._tpFilter
            $ tablePermissionsInfo remotePerms
nativeQueryRelationshipField ri = do
  relFieldName <- lift $ textToName $ relNameToTxt $ riName ri
  case riTarget ri of
    RelTargetNativeQuery nativeQueryName -> runMaybeT $ do
      nativeQueryInfo <- askNativeQueryInfo nativeQueryName

      let objectRelDesc = Just $ G.Description "An array relationship"
          arrayNullability = Nullable
          innerNullability = Nullable

      nativeQueryParser <-
        MaybeT $ selectNativeQuery nativeQueryInfo relFieldName arrayNullability objectRelDesc

      pure
        $ nativeQueryParser
        <&> \selectExp ->
          IR.AFArrayRelation
            $ IR.ASSimple
            $ IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) innerNullability selectExp
    RelTargetTable otherTableName -> runMaybeT $ do
      let arrayRelDesc = Just $ G.Description "An array relationship"

      otherTableInfo <- lift $ askTableInfo otherTableName
      otherTableParser <- MaybeT $ selectTable otherTableInfo relFieldName arrayRelDesc
      let arrayRelField =
            otherTableParser <&> \selectExp ->
              IR.AFArrayRelation
                $ IR.ASSimple
                $ IR.AnnRelationSelectG (riName ri) (unRelMapping $ riMapping ri) Nullable
                $ selectExp
      pure arrayRelField
