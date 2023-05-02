{-# LANGUAGE QuasiQuotes #-}

-- | Schema parsers for native queries.
module Hasura.NativeQuery.Schema (defaultBuildNativeQueryRootFields) where

import Data.Has (Has (getter))
import Data.HashMap.Strict qualified as HashMap
import Data.Monoid (Ap (Ap, getAp))
import Hasura.GraphQL.Schema.Backend
  ( BackendLogicalModelSelectSchema (..),
    BackendSchema (columnParser),
    MonadBuildSchema,
  )
import Hasura.GraphQL.Schema.Common
  ( SchemaT,
    retrieve,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.LogicalModel.Schema
import Hasura.NativeQuery.Cache (NativeQueryInfo (..))
import Hasura.NativeQuery.IR (NativeQuery (..))
import Hasura.NativeQuery.Metadata (ArgumentName (..), InterpolatedQuery (..))
import Hasura.NativeQuery.Types (NullableScalarType (..), getNativeQueryName)
import Hasura.Prelude
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Select (QueryDB (QDBMultipleRows))
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value (Provenance (FromInternal), UnpreparedValue (UVParameter), openValueOrigin)
import Hasura.RQL.Types.Column qualified as Column
import Hasura.RQL.Types.Metadata.Object qualified as MO
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.RQL.Types.Source
  ( SourceInfo (_siCustomization, _siName),
  )
import Hasura.RQL.Types.SourceCustomization
  ( ResolvedSourceCustomization (_rscNamingConvention),
  )
import Hasura.SQL.AnyBackend (mkAnyBackend)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

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
defaultBuildNativeQueryRootFields NativeQueryInfo {..} = runMaybeT $ do
  let fieldName = getNativeQueryName _nqiRootFieldName

  nativeQueryArgsParser <-
    nativeQueryArgumentsSchema @b @r @m @n fieldName _nqiArguments

  sourceInfo :: SourceInfo b <- asks getter

  let sourceName = _siName sourceInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
      description = G.Description <$> _nqiDescription

  stringifyNumbers <- retrieve Options.soStringifyNumbers

  logicalModelPermissions <-
    MaybeT . fmap Just $
      buildLogicalModelPermissions @b @r @m @n _nqiReturns

  (selectionSetParser, logicalModelsArgsParser) <-
    MaybeT $ buildLogicalModelFields _nqiArrayRelationships _nqiReturns

  let interpolatedQuery nqArgs =
        InterpolatedQuery $
          (fmap . fmap)
            ( \var@(ArgumentName name) -> case HashMap.lookup var nqArgs of
                Just arg -> UVParameter (FromInternal name) arg
                Nothing ->
                  -- the `nativeQueryArgsParser` will already have checked
                  -- we have all the args the query needs so this _should
                  -- not_ happen
                  error $ "No native query arg passed for " <> show var
            )
            (getInterpolatedQuery _nqiCode)

  let sourceObj =
        MO.MOSourceObjId
          sourceName
          (mkAnyBackend $ MO.SMONativeQuery @b _nqiRootFieldName)

  pure $
    P.setFieldParserOrigin sourceObj $
      P.subselection
        fieldName
        description
        ( (,)
            <$> logicalModelsArgsParser
            <*> nativeQueryArgsParser
        )
        selectionSetParser
        <&> \((lmArgs, nqArgs), fields) ->
          QDBMultipleRows $
            IR.AnnSelectG
              { IR._asnFields = fields,
                IR._asnFrom =
                  IR.FromNativeQuery
                    NativeQuery
                      { nqRootFieldName = _nqiRootFieldName,
                        nqArgs,
                        nqInterpolatedQuery = interpolatedQuery nqArgs,
                        nqLogicalModel = buildLogicalModelIR _nqiReturns
                      },
                IR._asnPerm = logicalModelPermissions,
                IR._asnArgs = lmArgs,
                IR._asnStrfyNum = stringifyNumbers,
                IR._asnNamingConvention = Just tCase
              }

nativeQueryArgumentsSchema ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  G.Name ->
  HashMap ArgumentName (NullableScalarType b) ->
  MaybeT (SchemaT r m) (P.InputFieldsParser n (HashMap ArgumentName (Column.ColumnValue b)))
nativeQueryArgumentsSchema nativeQueryName argsSignature = do
  -- Lift 'SchemaT r m (InputFieldsParser ..)' into a monoid using Applicative.
  -- This lets us use 'foldMap' + monoid structure of hashmaps to avoid awkwardly
  -- traversing the arguments and building the resulting parser.
  argsParser <-
    getAp $
      foldMap
        ( \(name, NullableScalarType {nstType, nstNullable, nstDescription}) -> Ap do
            argValueParser <-
              fmap (HashMap.singleton name . openValueOrigin)
                <$> lift (columnParser (Column.ColumnScalar nstType) (G.Nullability nstNullable))
            -- TODO: Naming conventions?
            -- TODO: Custom fields? (Probably not)
            argName <- hoistMaybe (G.mkName (getArgumentName name))
            let description = case nstDescription of
                  Just desc -> G.Description desc
                  Nothing -> G.Description ("Native query argument " <> getArgumentName name)
            pure $
              P.field
                argName
                (Just description)
                argValueParser
        )
        (HashMap.toList argsSignature)

  let desc = Just $ G.Description $ G.unName nativeQueryName <> " Native Query Arguments"

  pure $
    if null argsSignature
      then mempty
      else
        P.field
          [G.name|args|]
          desc
          (P.object (nativeQueryName <> [G.name|_arguments|]) desc argsParser)
