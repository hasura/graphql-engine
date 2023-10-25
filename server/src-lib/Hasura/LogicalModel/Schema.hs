{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Schema parsers for logical models
module Hasura.LogicalModel.Schema
  ( getSelPermInfoForLogicalModel,
    buildLogicalModelIR,
    buildLogicalModelPermissions,
    logicalModelOrderByArg,
    logicalModelSelectionList,
    logicalModelWhereArg,
    defaultLogicalModelArgs,
    defaultLogicalModelSelectionSet,
    logicalModelFieldParsers,
  )
where

import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as S
import Data.Text.Extended
import Data.Text.NonEmpty (mkNonEmptyText)
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Internal.Parser qualified as IP
import Hasura.GraphQL.Schema.Backend
  ( BackendLogicalModelSelectSchema (..),
    MonadBuildSchema,
    columnParser,
    scalarSelectionArgumentsParser,
  )
import Hasura.GraphQL.Schema.BoolExp
import Hasura.GraphQL.Schema.Common
  ( AnnotatedField,
    AnnotatedFields,
    SchemaT,
    SelectArgs,
    getRedactionExprForColumn,
    parsedSelectionsToFields,
    partialSQLExpToUnpreparedValue,
    retrieve,
    scRole,
  )
import Hasura.GraphQL.Schema.OrderBy
import Hasura.GraphQL.Schema.Parser
  ( InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select (defaultArgsParser)
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
import Hasura.LogicalModel.Common (getSelPermInfoForLogicalModel)
import Hasura.LogicalModel.IR (LogicalModel (..))
import Hasura.LogicalModel.Types
  ( LogicalModelField (..),
    LogicalModelName (..),
    LogicalModelType (..),
    LogicalModelTypeArray (..),
    LogicalModelTypeReference (..),
    LogicalModelTypeScalar (..),
  )
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.Types.Backend (Backend, Column)
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common (RelName (..))
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Relationships.Local (Nullable (..))
import Hasura.RQL.Types.Roles (RoleName)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache (SelPermInfo (..))
import Language.GraphQL.Draft.Syntax qualified as G

-- | build select permissions for logical model
logicalModelPermissions ::
  (Backend b) =>
  LogicalModelInfo b ->
  RoleName ->
  Maybe (IR.TablePermG b (IR.UnpreparedValue b))
logicalModelPermissions logicalModel roleName = do
  getSelPermInfoForLogicalModel roleName logicalModel <&> \selectPermissions ->
    IR.TablePerm
      { IR._tpFilter = fmap partialSQLExpToUnpreparedValue <$> spiFilter selectPermissions,
        IR._tpLimit = spiLimit selectPermissions
      }

-- | turn post-schema cache LogicalModelInfo into IR
buildLogicalModelIR :: LogicalModelInfo b -> LogicalModel b
buildLogicalModelIR LogicalModelInfo {..} =
  LogicalModel
    { lmName = _lmiName,
      lmFields = _lmiFields
    }

-- | top-level select permissions for a logical model
buildLogicalModelPermissions ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (Maybe (IR.TablePermG b (IR.UnpreparedValue b)))
buildLogicalModelPermissions logicalModel = do
  roleName <- retrieve scRole

  pure $ logicalModelPermissions logicalModel roleName

logicalModelColumnsForRole ::
  (Backend b) =>
  RoleName ->
  LogicalModelInfo b ->
  [(Column b, LogicalModelField b, IR.AnnRedactionExpUnpreparedValue b)]
logicalModelColumnsForRole roleName logicalModel =
  case getSelPermInfoForLogicalModel roleName logicalModel of
    Just selectPermissions ->
      _lmiFields logicalModel
        & InsOrdHashMap.toList
        & mapMaybe
          ( \(column, lmField) ->
              -- Only columns that have redaction expressions are selectable
              -- (because then they are defined in spiCols in selectPermissions)
              (column,lmField,) <$> getRedactionExprForColumn selectPermissions column
          )
    Nothing -> []

-- just the field parsers themselves
-- we use them (plus relationships etc) to make the Native Query / Stored
-- Procedure types
-- for now this is not memoized so please don't call it from
-- `parseLogicalModelField`, and instead call a top-level parser
logicalModelFieldParsers ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  S.Set RelName ->
  LogicalModelInfo b ->
  SchemaT r m [(IP.FieldParser MetadataObjId n (AnnotatedField b))]
logicalModelFieldParsers knownRelNames logicalModel = do
  roleName <- retrieve scRole
  let allowedColumns = logicalModelColumnsForRole roleName logicalModel

  -- we filter out parsers that return 'Nothing' as those we are no permitted to see.
  catMaybes <$> traverse (\(column, lmField, redactionExp) -> parseLogicalModelField knownRelNames column lmField redactionExp) allowedColumns

-- | this seems like it works on luck, ie that everything is really just Text
-- underneath
columnToRelName :: forall b. (Backend b) => Column b -> Maybe RelName
columnToRelName column =
  RelName <$> mkNonEmptyText (toTxt column)

-- | parse a single logical model field. Currently the only way to 'fulfil' a
-- non-scalar field is with a relationship that provides the nested
-- object/array information.
parseLogicalModelField ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  S.Set RelName ->
  Column b ->
  LogicalModelField b ->
  IR.AnnRedactionExpUnpreparedValue b ->
  SchemaT r m (Maybe (IP.FieldParser MetadataObjId n (AnnotatedField b)))
parseLogicalModelField knownRelNames column logimoField redactionExp = runMaybeT do
  case logimoField of
    ( LogicalModelField
        { lmfDescription,
          lmfType = LogicalModelTypeScalar (LogicalModelTypeScalarC {lmtsScalar, lmtsNullable})
        }
      ) -> do
        columnName <- G.mkName (toTxt column) `onNothing` throw500 (column <<> " is not a valid GraphQL name")

        let columnType = ColumnScalar lmtsScalar
            pathArg = scalarSelectionArgumentsParser columnType

        field <- lift $ columnParser columnType (G.Nullability lmtsNullable)

        pure
          $! P.selection columnName (G.Description <$> lmfDescription) pathArg field
          <&> IR.mkAnnColumnField column columnType redactionExp
    ( LogicalModelField
        { lmfType =
            LogicalModelTypeReference
              (LogicalModelTypeReferenceC {})
        }
      ) -> do
        -- we have no use for logical model object fields atm, but we don't
        -- want to explode if they're set up for relationships (as that used to
        -- be allowed)
        relName <- columnToRelName @b column `onNothing` throw500 (column <<> " is not a valid relationship name")

        if S.member relName knownRelNames
          then hoistMaybe Nothing -- a relationships exists with this name, gently ignore it to ensure backwards compat
          else
            throw500
              ( "Unexpected logical model object field '"
                  <> toTxt relName
                  <> "'."
              )
    ( LogicalModelField
        { lmfType =
            LogicalModelTypeArray
              ( LogicalModelTypeArrayC
                  { lmtaArray =
                      LogicalModelTypeReference (LogicalModelTypeReferenceC {})
                  }
                )
        }
      ) -> do
        -- we have no use for logical model array fields atm, but we don't
        -- want to explode if they're set up for relationships (as that used to
        -- be allowed)
        relName <- columnToRelName @b column `onNothing` throw500 (column <<> " is not a valid relationship name")

        if S.member relName knownRelNames
          then hoistMaybe Nothing -- a relationships exists with this name, gently ignore it to ensure backwards compat
          else
            throw500
              ( "Unexpected logical model array field '"
                  <> toTxt relName
                  <> "'."
              )
    ( LogicalModelField
        { lmfType =
            LogicalModelTypeArray
              (LogicalModelTypeArrayC {lmtaArray = LogicalModelTypeScalar _})
        }
      ) ->
        throw500 "Arrays of scalar types are not currently implemented"
    ( LogicalModelField
        { lmfType =
            LogicalModelTypeArray
              (LogicalModelTypeArrayC {lmtaArray = LogicalModelTypeArray _})
        }
      ) ->
        throw500 "Nested arrays are not currently implemented"

defaultLogicalModelSelectionSet ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (Maybe (Parser 'Output n (AnnotatedFields b)))
defaultLogicalModelSelectionSet logicalModel = runMaybeT do
  roleName <- retrieve scRole

  let allowedColumns = logicalModelColumnsForRole roleName logicalModel
      fieldName = getLogicalModelName (_lmiName logicalModel)
      description = G.Description <$> _lmiDescription logicalModel
      -- We entirely ignore Relay for now.
      implementsInterfaces = mempty

  lift $ P.memoizeOn 'defaultLogicalModelSelectionSet fieldName do
    -- we filter out parsers that return 'Nothing' as those we are no permitted to see.
    parsers <- catMaybes <$> traverse (\(column, lmField, redactionExp) -> parseLogicalModelField mempty column lmField redactionExp) allowedColumns

    pure
      $ P.selectionSetObject fieldName description parsers implementsInterfaces
      <&> parsedSelectionsToFields IR.AFExpression

logicalModelSelectionList ::
  (MonadBuildSchema b r m n, BackendLogicalModelSelectSchema b) =>
  Nullable ->
  LogicalModelInfo b ->
  SchemaT r m (Maybe (Parser 'Output n (AnnotatedFields b)))
logicalModelSelectionList nullability logicalModel =
  fmap nullabilityModifier <$> logicalModelSelectionSet logicalModel
  where
    nullabilityModifier =
      case nullability of
        Nullable -> nullableObjectList
        NotNullable -> nonNullableObjectList

    -- \| Converts an output type parser from object_type to [object_type!]!
    nonNullableObjectList :: Parser 'Output m a -> Parser 'Output m a
    nonNullableObjectList =
      P.nonNullableParser . P.multiple . P.nonNullableParser

    -- \| Converts an output type parser from object_type to [object_type!]
    nullableObjectList :: Parser 'Output m a -> Parser 'Output m a
    nullableObjectList =
      P.multiple . P.nonNullableParser

-- | Argument to filter rows returned from table selection
-- > where: table_bool_exp
logicalModelWhereArg ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (InputFieldsParser n (Maybe (IR.AnnBoolExp b (IR.UnpreparedValue b))))
logicalModelWhereArg logicalModel = do
  boolExpParser <- logicalModelBoolExp logicalModel
  pure
    $ fmap join
    $ P.fieldOptional whereName whereDesc
    $ P.nullable boolExpParser
  where
    whereName = Name._where
    whereDesc = Just $ G.Description "filter the rows returned"

-- | Argument to sort rows returned from table selection
-- > order_by: [table_order_by!]
logicalModelOrderByArg ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (InputFieldsParser n (Maybe (NonEmpty (IR.AnnotatedOrderByItemG b (IR.UnpreparedValue b)))))
logicalModelOrderByArg logicalModel = do
  tCase <- retrieve $ _rscNamingConvention . _siCustomization @b
  orderByParser <- logicalModelOrderByExp logicalModel
  let orderByName = applyFieldNameCaseCust tCase Name._order_by
      orderByDesc = Just $ G.Description "sort the rows by one or more columns"
  pure $ do
    maybeOrderByExps <-
      fmap join
        $ P.fieldOptional orderByName orderByDesc
        $ P.nullable
        $ P.list orderByParser
    pure $ maybeOrderByExps >>= NE.nonEmpty . concat

-- | Argument to distinct select on columns returned from table selection
-- > distinct_on: [table_select_column!]
logicalModelDistinctArg ::
  forall b r m n.
  ( MonadBuildSchema b r m n
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (InputFieldsParser n (Maybe (NonEmpty (IR.AnnDistinctColumn b (IR.UnpreparedValue b)))))
logicalModelDistinctArg logicalModel = do
  roleName <- retrieve scRole
  let selectPermissions = getSelPermInfoForLogicalModel roleName logicalModel

  let name = getLogicalModelName (_lmiName logicalModel)

  tCase <- retrieve $ _rscNamingConvention . _siCustomization @b

  let maybeColumnDefinitions =
        traverse (definitionFromTypeRow selectPermissions) (InsOrdHashMap.keys (_lmiFields logicalModel))
          >>= NE.nonEmpty

  case (,) <$> G.mkName "_enum_name" <*> maybeColumnDefinitions of
    Nothing -> throw500 $ "Error creating an enum name for logical model " <> tshow (_lmiName logicalModel)
    Just (enum', columnDefinitions) -> do
      let enumName = name <> enum'
          description = Nothing
          columnsEnum = Just $ P.enum @n enumName description columnDefinitions
          distinctOnName = applyFieldNameCaseCust tCase Name._distinct_on
          distinctOnDesc = Just $ G.Description "distinct select on columns"

      pure do
        maybeDistinctOnColumns <-
          join
            . join
            <$> for
              columnsEnum
              (P.fieldOptional distinctOnName distinctOnDesc . P.nullable . P.list)
        pure $ maybeDistinctOnColumns >>= NE.nonEmpty
  where
    definitionFromTypeRow :: Maybe (SelPermInfo b) -> Column b -> Maybe (P.Definition P.EnumValueInfo, IR.AnnDistinctColumn b (IR.UnpreparedValue b))
    definitionFromTypeRow maybeSelectPermissions name' = do
      selectPermissions <- maybeSelectPermissions
      columnName <- G.mkName (toTxt name')

      let definition =
            P.Definition
              { dName = columnName,
                dDescription = Just "column name",
                dOrigin = Nothing,
                dDirectives = mempty,
                dInfo = P.EnumValueInfo
              }

      let redactionExp = fromMaybe IR.NoRedaction $ getRedactionExprForColumn selectPermissions name'

      pure (definition, IR.AnnDistinctColumn name' redactionExp)

defaultLogicalModelArgs ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    AggregationPredicatesSchema b
  ) =>
  LogicalModelInfo b ->
  SchemaT r m (InputFieldsParser n (SelectArgs b))
defaultLogicalModelArgs logicalModel = do
  whereParser <- logicalModelWhereArg logicalModel
  orderByParser <- logicalModelOrderByArg logicalModel
  distinctParser <- logicalModelDistinctArg logicalModel

  defaultArgsParser whereParser orderByParser distinctParser
