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
import Hasura.GraphQL.Schema.Backend
  ( BackendLogicalModelSelectSchema (..),
    MonadBuildSchema,
  )
import Hasura.GraphQL.Schema.Common
  ( SchemaT,
    retrieve,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.LogicalModel.Schema (buildLogicalModelFields, buildLogicalModelIR, buildLogicalModelPermissions)
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
import Hasura.RQL.Types.Metadata.Object qualified as MO
import Hasura.RQL.Types.Relationships.Local (Nullable (..))
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
    BackendLogicalModelSelectSchema b
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
defaultSelectNativeQueryObject NativeQueryInfo {..} fieldName description = runMaybeT $ do
  nativeQueryArgsParser <-
    nativeQueryArgumentsSchema @b @r @m @n fieldName _nqiArguments

  sourceInfo :: SourceInfo b <- asks getter

  let sourceName = _siName sourceInfo

  logicalModelPermissions <-
    MaybeT
      . fmap Just
      $ buildLogicalModelPermissions @b @r @m @n _nqiReturns

  selectionSetParser <-
    MaybeT $ logicalModelSelectionSet _nqiRelationships _nqiReturns

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

-- | select a native query - implementation is the same for root fields and
-- array relationships
defaultSelectNativeQuery ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendLogicalModelSelectSchema b
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
defaultSelectNativeQuery NativeQueryInfo {..} fieldName nullability description = runMaybeT $ do
  nativeQueryArgsParser <-
    nativeQueryArgumentsSchema @b @r @m @n fieldName _nqiArguments

  sourceInfo :: SourceInfo b <- asks getter

  let sourceName = _siName sourceInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo

  stringifyNumbers <- retrieve Options.soStringifyNumbers

  logicalModelPermissions <-
    MaybeT
      . fmap Just
      $ buildLogicalModelPermissions @b @r @m @n _nqiReturns

  (selectionListParser, logicalModelsArgsParser) <-
    MaybeT $ buildLogicalModelFields _nqiRelationships nullability _nqiReturns

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
    BackendLogicalModelSelectSchema b
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
