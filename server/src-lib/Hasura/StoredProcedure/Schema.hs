{-# LANGUAGE QuasiQuotes #-}

-- | Schema parsers for stored procedures.
module Hasura.StoredProcedure.Schema (defaultBuildStoredProcedureRootFields) where

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
import Hasura.StoredProcedure.Cache (StoredProcedureInfo (..))
import Hasura.StoredProcedure.IR (StoredProcedure (..))
import Hasura.StoredProcedure.Metadata (ArgumentName (..), InterpolatedQuery (..))
import Hasura.StoredProcedure.Types (NullableScalarType (..), getStoredProcedureName)
import Language.GraphQL.Draft.Syntax qualified as G
import Language.GraphQL.Draft.Syntax.QQ qualified as G

defaultBuildStoredProcedureRootFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendLogicalModelSelectSchema b
  ) =>
  StoredProcedureInfo b ->
  SchemaT
    r
    m
    (Maybe (P.FieldParser n (QueryDB b (RemoteRelationshipField UnpreparedValue) (UnpreparedValue b))))
defaultBuildStoredProcedureRootFields StoredProcedureInfo {..} = runMaybeT $ do
  let fieldName = getStoredProcedureName _spiRootFieldName

  storedProcedureArgsParser <-
    storedProcedureArgumentsSchema @b @r @m @n fieldName _spiArguments

  sourceInfo :: SourceInfo b <- asks getter

  let sourceName = _siName sourceInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
      description = G.Description <$> _spiDescription

  stringifyNumbers <- retrieve Options.soStringifyNumbers

  logicalModelPermissions <-
    MaybeT . fmap Just $
      buildLogicalModelPermissions @b @r @m @n _spiReturns

  (selectionSetParser, logicalModelsArgsParser) <-
    MaybeT $ buildLogicalModelFields _spiArrayRelationships _spiReturns

  let interpolatedQuery spArgs =
        InterpolatedQuery $
          (fmap . fmap)
            ( \var@(ArgumentName name) -> case HashMap.lookup var spArgs of
                Just arg -> UVParameter (FromInternal name) arg
                Nothing ->
                  -- the `storedProcedureArgsParser` will already have checked
                  -- we have all the args the query needs so this _should
                  -- not_ happen
                  error $ "No stored procedure arg passed for " <> show var
            )
            (getInterpolatedQuery _spiCode)

  let sourceObj =
        MO.MOSourceObjId
          sourceName
          (mkAnyBackend $ MO.SMOStoredProcedure @b _spiRootFieldName)

  pure $
    P.setFieldParserOrigin sourceObj $
      P.subselection
        fieldName
        description
        ( (,)
            <$> logicalModelsArgsParser
            <*> storedProcedureArgsParser
        )
        selectionSetParser
        <&> \((lmArgs, spArgs), fields) ->
          QDBMultipleRows $
            IR.AnnSelectG
              { IR._asnFields = fields,
                IR._asnFrom =
                  IR.FromStoredProcedure
                    StoredProcedure
                      { spRootFieldName = _spiRootFieldName,
                        spArgs,
                        spInterpolatedQuery = interpolatedQuery spArgs,
                        spLogicalModel = buildLogicalModelIR _spiReturns
                      },
                IR._asnPerm = logicalModelPermissions,
                IR._asnArgs = lmArgs,
                IR._asnStrfyNum = stringifyNumbers,
                IR._asnNamingConvention = Just tCase
              }

storedProcedureArgumentsSchema ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  G.Name ->
  HashMap ArgumentName (NullableScalarType b) ->
  MaybeT (SchemaT r m) (P.InputFieldsParser n (HashMap ArgumentName (Column.ColumnValue b)))
storedProcedureArgumentsSchema storedProcedureName argsSignature = do
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
                  Nothing -> G.Description ("Stored procedure argument " <> getArgumentName name)
            pure $
              P.field
                argName
                (Just description)
                argValueParser
        )
        (HashMap.toList argsSignature)

  let desc = Just $ G.Description $ G.unName storedProcedureName <> " Stored Procedure Arguments"

  pure $
    if null argsSignature
      then mempty
      else
        P.field
          [G.name|args|]
          desc
          (P.object (storedProcedureName <> [G.name|_arguments|]) desc argsParser)
