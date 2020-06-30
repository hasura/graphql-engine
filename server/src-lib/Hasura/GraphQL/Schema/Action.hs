module Hasura.GraphQL.Schema.Action
  ( actionExecute
  , actionAsyncMutation
  , actionAsyncQuery
  ) where

import           Hasura.Prelude
import           Data.Has

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map
import qualified Language.GraphQL.Draft.Syntax         as G

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P
import qualified Hasura.RQL.DML.Internal               as RQL
import qualified Hasura.RQL.DML.Select.Types           as RQL

import           Hasura.GraphQL.Parser                 (FieldParser, InputFieldsParser, Kind (..),
                                                        Parser, UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.RQL.Types
import           Hasura.SQL.Types

actionExecute
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => NonObjectTypeMap
  -> ActionInfo
  -> m (Maybe (FieldParser n (AnnActionExecution UnpreparedValue)))
actionExecute nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- lift askRoleName
  guard $ roleName == adminRole || roleName `Map.member` permissions
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  selectionSet <- actionOutputFields outputObject
  stringifyNum <- asks $ qcStringifyNum . getter
  pure $ P.subselection fieldName description inputArguments selectionSet
         <&> \(argsJson, fields) -> AnnActionExecution
               { _aaeName = actionName
               , _aaeFields = fields
               , _aaePayload = argsJson
               , _aaeOutputType = _adOutputType definition
               , _aaeOutputFields = getActionOutputFields outputObject
               , _aaeDefinitionList = mkDefinitionList outputObject
               , _aaeWebhook = _adHandler definition
               , _aaeHeaders = _adHeaders definition
               , _aaeForwardClientHeaders = _adForwardClientHeaders definition
               , _aaeStrfyNum = stringifyNum
               }
  where
    ActionInfo actionName outputObject definition permissions comment = actionInfo

actionAsyncMutation
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => NonObjectTypeMap
  -> ActionInfo
  -> m (Maybe (FieldParser n AnnActionMutationAsync))
actionAsyncMutation nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- lift askRoleName
  guard $ roleName == adminRole || roleName `Map.member` permissions
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  actionId <- lift actionIdParser
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  pure $ P.selection fieldName description inputArguments actionId
         <&> AnnActionMutationAsync actionName
  where
    ActionInfo actionName _ definition permissions comment = actionInfo

actionAsyncQuery
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => ActionInfo
  -> m (Maybe (FieldParser n (AnnActionAsyncQuery UnpreparedValue)))
actionAsyncQuery actionInfo = runMaybeT do
  roleName <- lift askRoleName
  guard $ roleName == adminRole || roleName `Map.member` permissions
  actionId <- lift actionIdParser
  actionOutputParser <- actionOutputFields outputObject
  createdAtFieldParser <-
    lift $ P.column (PGColumnScalar PGTimeStampTZ) (G.Nullability False)
  errorsFieldParser <-
    lift $ P.column (PGColumnScalar PGJSON) (G.Nullability True)

  let fieldName = unActionName actionName
      description = G.Description <$> comment
      actionIdInputField =
        P.field idFieldName (Just idFieldDescription) actionId
      allFieldParsers =
        let idField        = P.selection_ idFieldName (Just idFieldDescription) actionId $> AsyncId
            createdAtField = P.selection_ $$(G.litName "created_at")
                             (Just "the time at which this action was created")
                             createdAtFieldParser $> AsyncCreatedAt
            errorsField    = P.selection_ $$(G.litName "errors")
                             (Just "errors related to the invocation")
                             errorsFieldParser $> AsyncErrors
            outputField    = P.subselection_ $$(G.litName "output")
                             (Just "the output fields of this action")
                             actionOutputParser <&> AsyncOutput
        in [idField, createdAtField, errorsField, outputField]
      selectionSet =
        let outputTypeName = unActionName actionName
            desc = G.Description $ "fields of action: " <>> actionName
        in P.selectionSet outputTypeName (Just desc) allFieldParsers
           <&> parsedSelectionsToFields AsyncTypename

  stringifyNum <- asks $ qcStringifyNum . getter
  pure $ P.subselection fieldName description actionIdInputField selectionSet
         <&> \(idArg, fields) -> AnnActionAsyncQuery
              { _aaaqName = actionName
              , _aaaqActionId = idArg
              , _aaaqOutputType = _adOutputType definition
              , _aaaqFields = fields
              , _aaaqDefinitionList = mkDefinitionList outputObject
              , _aaaqStringifyNum = stringifyNum
              }
  where
    ActionInfo actionName outputObject definition permissions comment = actionInfo
    idFieldName = $$(G.litName "id")
    idFieldDescription = "the unique id of an action"

-- | Async action's unique id
actionIdParser
  :: (MonadSchema n m, MonadError QErr m)
  => m (Parser 'Both n UnpreparedValue)
actionIdParser =
  fmap P.mkParameter <$> P.column (PGColumnScalar PGUUID) (G.Nullability False)

actionOutputFields
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => AnnotatedObjectType
  -> MaybeT m (Parser 'Output n (RQL.AnnFldsG UnpreparedValue))
actionOutputFields outputObject = do
  let scalarOrEnumFields = map scalarOrEnumFieldParser $ toList $ _otdFields outputObject
  relationshipFields <- forM (_otdRelationships outputObject) $ traverse relationshipFieldParser
  let allFieldParsers = scalarOrEnumFields <>
                        maybe [] toList relationshipFields
      outputTypeName = unObjectTypeName $ _otdName outputObject
      outputTypeDescription = _otdDescription outputObject
  pure $ P.selectionSet outputTypeName outputTypeDescription allFieldParsers
         <&> parsedSelectionsToFields RQL.FExp
  where
    scalarOrEnumFieldParser
      :: ObjectFieldDefinition (G.GType, AnnotatedObjectFieldType)
      -> FieldParser n (RQL.AnnFldG UnpreparedValue)
    scalarOrEnumFieldParser (ObjectFieldDefinition name _ description ty) =
      let (gType, objectFieldType) = ty
          fieldName = unObjectFieldName name
          -- FIXME?
          pgColumnInfo = PGColumnInfo (unsafePGCol $ G.unName fieldName)
                         fieldName 0 (PGColumnScalar PGJSON) (G.isNullable gType) Nothing
          fieldParser = case objectFieldType of
            AOFTScalar def -> customScalarParser def
            AOFTEnum def   -> customEnumParser def
      in bool P.nonNullableField id (G.isNullable gType) $
         P.selection_ (unObjectFieldName name) description fieldParser
         $> RQL.mkAnnColField pgColumnInfo Nothing

    relationshipFieldParser
      :: TypeRelationship TableInfo PGColumnInfo
      -> MaybeT m (FieldParser n (RQL.AnnFldG UnpreparedValue))
    relationshipFieldParser typeRelationship = do
      let TypeRelationship relName relType tableInfo fieldMapping = typeRelationship
          tableName = _tciName $ _tiCoreInfo tableInfo
          fieldName = unRelationshipName relName
      roleName <- lift askRoleName
      tablePerms <- MaybeT $ pure $ RQL.getPermInfoMaybe roleName PASelect tableInfo
      tableParser <- lift $ selectTable tableName fieldName Nothing tablePerms
      pure $ tableParser <&> \selectExp ->
        let tableRelName = RelName $ mkNonEmptyTextUnsafe $ G.unName fieldName
            columnMapping = Map.fromList $
              [ (unsafePGCol $ G.unName $ unObjectFieldName k, pgiColumn v)
              | (k, v) <- Map.toList fieldMapping
              ]
            annotatedRelationship = RQL.AnnRelG tableRelName columnMapping selectExp
        in case relType of
             ObjRel -> RQL.FObj annotatedRelationship
             ArrRel -> RQL.FArr $ RQL.ASSimple annotatedRelationship

mkDefinitionList :: AnnotatedObjectType -> [(PGCol, PGScalarType)]
mkDefinitionList annotatedOutputType =
  [ (unsafePGCol $ G.unName k,  PGJSON) -- FIXME? - Use relationship mapping references
  | k <- map (unObjectFieldName . _ofdName) $
         toList $ _otdFields annotatedOutputType
  ]

-- This should into schema/action.hs
actionInputArguments
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m)
  => NonObjectTypeMap
  -> [ArgumentDefinition (G.GType, NonObjectCustomType)]
  -> m (InputFieldsParser n J.Value)
actionInputArguments nonObjectTypeMap arguments = do
  argumentParsers <- for arguments $ \argument -> do
    let ArgumentDefinition argumentName (gType, nonObjectType) argumentDescription = argument
        name = unArgumentName argumentName
    (name,) <$> argumentParser name argumentDescription gType nonObjectType
  pure $ J.Object <$> inputFieldsToObject argumentParsers
  where
    inputFieldsToObject
      :: [(G.Name, InputFieldsParser n (Maybe J.Value))]
      -> InputFieldsParser n J.Object
    inputFieldsToObject inputFields =
      let mkTuple (name, parser) = fmap (G.unName name,) <$> parser
      in fmap (Map.fromList . catMaybes) $ sequenceA $ map mkTuple inputFields

    argumentParser
      :: G.Name
      -> Maybe G.Description
      -> G.GType
      -> NonObjectCustomType
      -> m (InputFieldsParser n (Maybe J.Value))
    argumentParser name description gType nonObjectType =
      case nonObjectType of
        NOCTScalar def -> pure $ P.fieldOptional name description $ mkParserModifier gType $ customScalarParser def
        NOCTEnum def -> pure $ P.fieldOptional name description $ mkParserModifier gType $ customEnumParser def
        NOCTInputObject def -> do
          let InputObjectTypeDefinition typeName objectDescription inputFields = def
              objectName = unInputObjectTypeName typeName
          inputFieldsParsers <- forM (toList inputFields) $ \inputField -> do
            let InputObjectFieldName fieldName = _iofdName inputField
                GraphQLType fieldType = _iofdType inputField
            nonObjectFieldType <-
              onNothing (Map.lookup (G.getBaseType fieldType) nonObjectTypeMap) $
                throw500 "object type for a field found in custom input object type"
            (fieldName,) <$> argumentParser fieldName (_iofdDescription inputField) fieldType nonObjectFieldType

          pure $ P.fieldOptional name description $ mkParserModifier gType $
                 P.object objectName objectDescription $
                 J.Object <$> inputFieldsToObject inputFieldsParsers

mkParserModifier
  :: (MonadParse m, 'Input P.<: k)
  => G.GType -> Parser k m J.Value -> Parser k m J.Value
mkParserModifier = \case
  G.TypeNamed nullable _    -> nullableModifier nullable
  G.TypeList nullable gType ->
    nullableModifier nullable . fmap J.toJSON . P.list . mkParserModifier gType
  where
    nullableModifier =
      bool (fmap J.toJSON) (fmap J.toJSON . P.nullable) . G.unNullability

customScalarParser
  :: MonadParse m
  => ScalarTypeDefinition -> Parser 'Both m J.Value
customScalarParser (ScalarTypeDefinition name description) =
  P.scalar name description P.SRAny

customEnumParser
  :: MonadParse m
  => EnumTypeDefinition -> Parser 'Both m J.Value
customEnumParser (EnumTypeDefinition typeName description enumValues) =
  let enumName = unEnumTypeName typeName
      enumValueDefinitions = enumValues <&> \enumValue ->
        let valueName = G.unEnumValue $ _evdValue enumValue
        in (, J.toJSON valueName) $ P.mkDefinition valueName
           (_evdDescription enumValue) P.EnumValueInfo
  in P.enum enumName description enumValueDefinitions
