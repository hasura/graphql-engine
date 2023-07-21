-- | Schema parsers for stored procedures.
module Hasura.StoredProcedure.Schema (defaultBuildStoredProcedureRootFields) where

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
import Hasura.LogicalModel.Schema (buildLogicalModelIR, buildLogicalModelPermissions, logicalModelSelectionList)
import Hasura.LogicalModelResolver.Schema (argumentsSchema)
import Hasura.Prelude
import Hasura.RQL.IR.Root (RemoteRelationshipField)
import Hasura.RQL.IR.Select (QueryDB (QDBMultipleRows))
import Hasura.RQL.IR.Select qualified as IR
import Hasura.RQL.IR.Value (Provenance (FromInternal), UnpreparedValue (UVParameter))
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
import Hasura.StoredProcedure.Cache (StoredProcedureInfo (..))
import Hasura.StoredProcedure.IR (StoredProcedure (..))
import Hasura.StoredProcedure.Metadata (ArgumentName (..))
import Hasura.StoredProcedure.Types (NullableScalarType (..))
import Language.GraphQL.Draft.Syntax qualified as G

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
  let fieldName = _spiGraphqlName

  storedProcedureArgsParser <-
    storedProcedureArgumentsSchema @b @r @m @n fieldName _spiArguments

  sourceInfo :: SourceInfo b <- asks getter

  let sourceName = _siName sourceInfo
      tCase = _rscNamingConvention $ _siCustomization sourceInfo
      description = G.Description <$> _spiDescription

  stringifyNumbers <- retrieve Options.soStringifyNumbers

  logicalModelPermissions <-
    MaybeT
      $ buildLogicalModelPermissions @b @r @m @n _spiReturns

  selectionListParser <- MaybeT $ logicalModelSelectionList @b @r @m @n NotNullable _spiReturns
  logicalModelsArgsParser <- lift $ logicalModelArguments @b @r @m @n _spiReturns

  let arguments spArgs =
        HashMap.mapWithKey
          ( \(ArgumentName name) val ->
              case Column.cvType val of
                Column.ColumnScalar st ->
                  (st, UVParameter (FromInternal name) val)
                Column.ColumnEnumReference {} ->
                  -- should not happen
                  error "Enums are unsupported in stored procedures."
          )
          spArgs

  let sourceObj =
        MO.MOSourceObjId
          sourceName
          (mkAnyBackend $ MO.SMOStoredProcedure @b _spiStoredProcedure)

  pure
    $ P.setFieldParserOrigin sourceObj
    $ P.subselection
      fieldName
      description
      ( (,)
          <$> logicalModelsArgsParser
          <*> storedProcedureArgsParser
      )
      selectionListParser
    <&> \((lmArgs, spArgs), fields) ->
      QDBMultipleRows
        $ IR.AnnSelectG
          { IR._asnFields = fields,
            IR._asnFrom =
              IR.FromStoredProcedure
                StoredProcedure
                  { spStoredProcedure = _spiStoredProcedure,
                    spGraphqlName = _spiGraphqlName,
                    spArgs = arguments spArgs,
                    spLogicalModel = buildLogicalModelIR _spiReturns
                  },
            IR._asnPerm = logicalModelPermissions,
            IR._asnArgs = lmArgs,
            IR._asnStrfyNum = stringifyNumbers,
            IR._asnNamingConvention = Just tCase
          }

storedProcedureArgumentsSchema ::
  forall b r m n.
  (MonadBuildSchema b r m n) =>
  G.Name ->
  HashMap ArgumentName (NullableScalarType b) ->
  MaybeT (SchemaT r m) (P.InputFieldsParser n (HashMap ArgumentName (Column.ColumnValue b)))
storedProcedureArgumentsSchema = argumentsSchema "Stored Procedure"
