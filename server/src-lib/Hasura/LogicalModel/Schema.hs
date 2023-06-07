{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Schema parsers for logical models
module Hasura.LogicalModel.Schema
  ( buildLogicalModelIR,
    buildLogicalModelPermissions,
    buildLogicalModelFields,
    defaultLogicalModelArgs,
    defaultLogicalModelSelectionSet,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.List.NonEmpty qualified as NE
import Data.Text.Extended
import Data.Text.NonEmpty (mkNonEmptyText)
import Hasura.Base.Error
import Hasura.GraphQL.Parser.Internal.Parser qualified as IP
import Hasura.GraphQL.Schema.Backend
  ( BackendLogicalModelSelectSchema (..),
    BackendNativeQuerySelectSchema (..),
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
    askNativeQueryInfo,
    parsedSelectionsToFields,
    partialSQLExpToUnpreparedValue,
    retrieve,
    scRole,
    textToName,
  )
import Hasura.GraphQL.Schema.OrderBy
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.GraphQL.Schema.Select (defaultArgsParser)
import Hasura.LogicalModel.Cache (LogicalModelInfo (..))
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
import Hasura.NativeQuery.Cache (NativeQueryInfo (..))
import Hasura.Prelude
import Hasura.RQL.IR qualified as IR
import Hasura.RQL.IR.BoolExp (gBoolExpTrue)
import Hasura.RQL.Types.Backend (Backend, Column)
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common (RelName (..), RelType (..), relNameToTxt)
import Hasura.RQL.Types.Metadata.Object
import Hasura.RQL.Types.Permission qualified as Permission
import Hasura.RQL.Types.Relationships.Local (Nullable (..), RelInfo (..), RelTarget (..))
import Hasura.RQL.Types.Roles (RoleName, adminRoleName)
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Table.Cache (SelPermInfo (..), _permSel)
import Language.GraphQL.Draft.Syntax qualified as G

-- | find list of columns we're allowed to access for this role
getSelPermInfoForLogicalModel ::
  RoleName ->
  LogicalModelInfo b ->
  Maybe (SelPermInfo b)
getSelPermInfoForLogicalModel role logicalModel =
  HashMap.lookup role (_lmiPermissions logicalModel) >>= _permSel

-- | build select permissions for logical model
-- `admin` can always select everything
logicalModelPermissions ::
  (Backend b) =>
  LogicalModelInfo b ->
  RoleName ->
  IR.TablePermG b (IR.UnpreparedValue b)
logicalModelPermissions logicalModel roleName = do
  if roleName == adminRoleName
    then IR.TablePerm gBoolExpTrue Nothing
    else case getSelPermInfoForLogicalModel roleName logicalModel of
      Just selectPermissions ->
        IR.TablePerm
          { IR._tpFilter = fmap partialSQLExpToUnpreparedValue <$> spiFilter selectPermissions,
            IR._tpLimit = spiLimit selectPermissions
          }
      Nothing -> IR.TablePerm gBoolExpTrue Nothing

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
  SchemaT r m (IR.TablePermG b (IR.UnpreparedValue b))
buildLogicalModelPermissions logicalModel = do
  roleName <- retrieve scRole

  pure $ logicalModelPermissions logicalModel roleName

-- in order to construct meaningful IR, we can only parse a Logical Model
-- in the context of it's use, therefore we must pass in any information on
-- relationships (and then,
buildLogicalModelFields ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendLogicalModelSelectSchema b
  ) =>
  InsOrdHashMap RelName (RelInfo b) ->
  Nullable ->
  LogicalModelInfo b ->
  SchemaT
    r
    m
    ( Maybe
        ( P.Parser 'P.Output n (AnnotatedFields b),
          P.InputFieldsParser n (IR.SelectArgsG b (IR.UnpreparedValue b))
        )
    )
buildLogicalModelFields relationshipInfo nullability logicalModel = runMaybeT $ do
  selectionSetParser <- MaybeT $ logicalModelSelectionList @b @r @m @n relationshipInfo nullability logicalModel
  logicalModelsArgsParser <- lift $ logicalModelArguments @b @r @m @n logicalModel

  pure (selectionSetParser, logicalModelsArgsParser)

logicalModelColumnsForRole ::
  RoleName ->
  LogicalModelInfo b ->
  Maybe (Permission.PermColSpec b)
logicalModelColumnsForRole role logicalModel =
  if role == adminRoleName
    then -- if admin, assume all columns are OK
      pure Permission.PCStar
    else -- find list of columns we're allowed to access for this role

      HashMap.lookup role (_lmiPermissions logicalModel)
        >>= _permSel
        <&> Permission.PCCols
        . HashMap.keys
        . spiCols

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
  ( MonadBuildSchema b r m n,
    BackendNativeQuerySelectSchema b
  ) =>
  InsOrdHashMap RelName (RelInfo b) ->
  Column b ->
  LogicalModelField b ->
  SchemaT r m (Maybe (IP.FieldParser MetadataObjId n (AnnotatedField b)))
parseLogicalModelField relationshipInfo column logimoField = runMaybeT do
  case logimoField of
    ( LogicalModelField
        { lmfDescription,
          lmfType = LogicalModelTypeScalar (LogicalModelTypeScalarC {lmtsScalar, lmtsNullable})
        }
      ) -> do
        columnName <- G.mkName (toTxt column) `onNothing` throw500 (column <<> " is not a valid GraphQL name")

        -- We have not yet worked out what providing permissions here enables
        let caseBoolExpUnpreparedValue = Nothing
            columnType = ColumnScalar lmtsScalar
            pathArg = scalarSelectionArgumentsParser columnType

        field <- lift $ columnParser columnType (G.Nullability lmtsNullable)

        pure
          $! P.selection columnName (G.Description <$> lmfDescription) pathArg field
          <&> IR.mkAnnColumnField column columnType caseBoolExpUnpreparedValue
    ( LogicalModelField
        { lmfType =
            LogicalModelTypeReference
              (LogicalModelTypeReferenceC {lmtrNullable, lmtrReference})
        }
      ) -> do
        relName <- columnToRelName @b column `onNothing` throw500 (column <<> " is not a valid relationship name")

        -- lookup the reference in the data source
        relationship <-
          InsOrdHashMap.lookup relName relationshipInfo
            `onNothing` throw500
              ( "Unexpected relationship name "
                  <> toTxt relName
                  <> ". Expecting one of: ["
                  <> commaSeparated (map relNameToTxt (InsOrdHashMap.keys relationshipInfo))
                  <> "]."
              )
        MaybeT $ logicalModelObjectRelationshipField @b @r @m @n lmtrReference (nullableFromBool lmtrNullable) relationship
    ( LogicalModelField
        { lmfType =
            LogicalModelTypeArray
              ( LogicalModelTypeArrayC
                  { lmtaArray =
                      LogicalModelTypeReference (LogicalModelTypeReferenceC {lmtrReference, lmtrNullable = innerNullability}),
                    lmtaNullable = arrayNullability
                  }
                )
        }
      ) -> do
        -- we currently ignore nullability and assume the field is
        -- non-nullable, as are the contents
        relName <- columnToRelName @b column `onNothing` throw500 (column <<> " is not a valid relationship name")
        -- lookup the reference in the data source

        relationship <- InsOrdHashMap.lookup relName relationshipInfo `onNothing` throw500 (relName <<> " is not a known relationship")

        MaybeT
          $ logicalModelArrayRelationshipField @b @r @m @n
            lmtrReference
            (nullableFromBool arrayNullability)
            (nullableFromBool innerNullability)
            relationship
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

nullableFromBool :: Bool -> Nullable
nullableFromBool True = Nullable
nullableFromBool False = NotNullable

defaultLogicalModelSelectionSet ::
  forall b r m n.
  ( MonadBuildSchema b r m n,
    BackendNativeQuerySelectSchema b
  ) =>
  InsOrdHashMap RelName (RelInfo b) ->
  LogicalModelInfo b ->
  SchemaT r m (Maybe (Parser 'Output n (AnnotatedFields b)))
defaultLogicalModelSelectionSet relationshipInfo logicalModel = runMaybeT do
  roleName <- retrieve scRole

  selectableColumns <- hoistMaybe $ logicalModelColumnsForRole roleName logicalModel

  let isSelectable column =
        case selectableColumns of
          Permission.PCStar -> True
          Permission.PCCols cols -> column `elem` cols

  let fieldName = getLogicalModelName (_lmiName logicalModel)

  -- which columns are we allowed to access given permissions?
  let allowedColumns =
        filter
          (isSelectable . fst)
          (InsOrdHashMap.toList (_lmiFields logicalModel))

  let description = G.Description <$> _lmiDescription logicalModel

      -- We entirely ignore Relay for now.
      implementsInterfaces = mempty

  lift $ P.memoizeOn 'defaultLogicalModelSelectionSet (InsOrdHashMap.toList relationshipInfo, fieldName) do
    -- we filter out parsers that return 'Nothing' as those we are no permitted to see.
    parsers <- catMaybes <$> traverse (uncurry (parseLogicalModelField relationshipInfo)) allowedColumns

    pure
      $ P.selectionSetObject fieldName description parsers implementsInterfaces
      <&> parsedSelectionsToFields IR.AFExpression

logicalModelSelectionList ::
  (MonadBuildSchema b r m n, BackendLogicalModelSelectSchema b) =>
  InsOrdHashMap RelName (RelInfo b) ->
  Nullable ->
  LogicalModelInfo b ->
  SchemaT r m (Maybe (Parser 'Output n (AnnotatedFields b)))
logicalModelSelectionList relationshipInfo nullability logicalModel =
  fmap nullabilityModifier <$> logicalModelSelectionSet relationshipInfo logicalModel
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
  SchemaT r m (InputFieldsParser n (Maybe (NonEmpty (Column b))))
logicalModelDistinctArg logicalModel = do
  let name = getLogicalModelName (_lmiName logicalModel)

  tCase <- retrieve $ _rscNamingConvention . _siCustomization @b

  let maybeColumnDefinitions =
        traverse definitionFromTypeRow (InsOrdHashMap.keys (_lmiFields logicalModel))
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
    definitionFromTypeRow :: Column b -> Maybe (P.Definition P.EnumValueInfo, Column b)
    definitionFromTypeRow name' = do
      columnName <- G.mkName (toTxt name')

      let definition =
            P.Definition
              { dName = columnName,
                dDescription = Just "column name",
                dOrigin = Nothing,
                dDirectives = mempty,
                dInfo = P.EnumValueInfo
              }
      pure (definition, name')

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

-- | Field parsers for a logical model object relationship
logicalModelObjectRelationshipField ::
  forall b r m n.
  ( BackendNativeQuerySelectSchema b,
    MonadBuildSchema b r m n
  ) =>
  LogicalModelName ->
  Nullable ->
  RelInfo b ->
  SchemaT r m (Maybe (FieldParser n (AnnotatedField b)))
logicalModelObjectRelationshipField logicalModelName nullability ri | riType ri == ObjRel = runMaybeT do
  case riTarget ri of
    RelTargetNativeQuery nativeQueryName -> do
      nativeQueryInfo <- askNativeQueryInfo nativeQueryName

      -- not sure if this the correct way to report mismatches, or if it
      -- even possible for this to be an issue at this point
      when
        (logicalModelName /= _lmiName (_nqiReturns nativeQueryInfo))
        ( throw500
            $ "Expected object relationship to return "
            <> toTxt logicalModelName
            <> " but it returns "
            <> toTxt (_lmiName (_nqiReturns nativeQueryInfo))
            <> "."
        )

      relFieldName <- lift $ textToName $ relNameToTxt $ riName ri

      let objectRelDesc = Just $ G.Description "An object relationship"

      nativeQueryParser <-
        MaybeT $ selectNativeQueryObject nativeQueryInfo relFieldName objectRelDesc

      -- this only affects the generated GraphQL type
      let nullabilityModifier =
            case nullability of
              Nullable -> id
              NotNullable -> IP.nonNullableField

      pure
        $ nullabilityModifier
        $ nativeQueryParser
        <&> \selectExp ->
          IR.AFObjectRelation (IR.AnnRelationSelectG (riName ri) (riMapping ri) nullability selectExp)
    RelTargetTable _otherTableName -> do
      throw500 "Object relationships from logical models to tables are not implemented"
logicalModelObjectRelationshipField _ _ _ =
  throw500 "the target logical model expected an object relationship, but this was an array"

-- | Field parsers for a logical model relationship
logicalModelArrayRelationshipField ::
  forall b r m n.
  ( BackendNativeQuerySelectSchema b,
    MonadBuildSchema b r m n
  ) =>
  LogicalModelName ->
  Nullable ->
  Nullable ->
  RelInfo b ->
  SchemaT r m (Maybe (FieldParser n (AnnotatedField b)))
logicalModelArrayRelationshipField logicalModelName arrayNullability innerNullability ri | riType ri == ArrRel = runMaybeT do
  case riTarget ri of
    RelTargetNativeQuery nativeQueryName -> do
      nativeQueryInfo <- askNativeQueryInfo nativeQueryName
      relFieldName <- lift $ textToName $ relNameToTxt $ riName ri

      -- not sure if this the correct way to report mismatches, or if it
      -- even possible for this to be an issue at this point
      when
        (logicalModelName /= _lmiName (_nqiReturns nativeQueryInfo))
        ( throw500
            $ "Expected array relationship to return "
            <> toTxt logicalModelName
            <> " but it returns "
            <> toTxt (_lmiName (_nqiReturns nativeQueryInfo))
            <> "."
        )

      let objectRelDesc = Just $ G.Description "An array relationship"

      nativeQueryParser <-
        MaybeT $ selectNativeQuery nativeQueryInfo relFieldName arrayNullability objectRelDesc
      pure
        $ nativeQueryParser
        <&> \selectExp ->
          IR.AFArrayRelation
            $ IR.ASSimple
            $ IR.AnnRelationSelectG (riName ri) (riMapping ri) innerNullability selectExp
    RelTargetTable _otherTableName -> do
      throw500 "Array relationships from logical models to tables are not implemented"
logicalModelArrayRelationshipField _ _ _ _ =
  throw500 "the target logical model expected an array relationship, but this was an object"
