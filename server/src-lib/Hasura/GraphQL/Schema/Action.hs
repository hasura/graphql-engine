module Hasura.GraphQL.Schema.Action
  ( actionExecute
  , actionAsyncMutation
  , actionAsyncQuery
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map
import qualified Language.GraphQL.Draft.Syntax         as G

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P
import qualified Hasura.RQL.DML.Internal               as RQL
import qualified Hasura.RQL.IR.Select                  as RQL

import           Data.Text.Extended
import           Data.Text.NonEmpty
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.SQL.Value
import           Hasura.GraphQL.Parser                 (FieldParser, InputFieldsParser, Kind (..),
                                                        Parser, UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Common
import           Hasura.GraphQL.Schema.Select
import           Hasura.RQL.Types
import           Hasura.Session


-- | actionExecute is used to execute either a query action or a synchronous
--   mutation action. A query action or a synchronous mutation action accepts
--   the field name and input arguments and a selectionset. The
--   input argument and selectionset types are defined by the user.
--
--
-- > action_name(action_input_arguments) {
-- >   col1: col1_type
-- >   col2: col2_type
-- > }
actionExecute
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => NonObjectTypeMap
  -> ActionInfo 'Postgres
  -> m (Maybe (FieldParser n (AnnActionExecution 'Postgres UnpreparedValue)))
actionExecute nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- lift askRoleName
  guard $ roleName == adminRoleName || roleName `Map.member` permissions
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  selectionSet <- lift $ actionOutputFields outputObject
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
               , _aaeTimeOut = _adTimeout definition
               }
  where
    ActionInfo actionName outputObject definition permissions comment = actionInfo

-- | actionAsyncMutation is used to execute a asynchronous mutation action. An
--   asynchronous action expects the field name and the input arguments to the
--   action. A selectionset is *not* expected. An action ID (UUID) will be
--   returned after performing the action
--
-- > action_name(action_input_arguments)
actionAsyncMutation
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => NonObjectTypeMap
  -> ActionInfo 'Postgres
  -> m (Maybe (FieldParser n AnnActionMutationAsync))
actionAsyncMutation nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- lift askRoleName
  guard $ roleName == adminRoleName || roleName `Map.member` permissions
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  actionId <- lift actionIdParser
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  pure $ P.selection fieldName description inputArguments actionId
         <&> AnnActionMutationAsync actionName
  where
    ActionInfo actionName _ definition permissions comment = actionInfo

-- | actionAsyncQuery is used to query/subscribe to the result of an
--   asynchronous mutation action. The only input argument to an
--   asynchronous mutation action is the action ID (UUID) and a selection
--   set is expected, the selection set contains 4 fields namely 'id',
--   'created_at','errors' and 'output'. The result of the action can be queried
--   through the 'output' field.
--
-- > action_name (id: UUID!) {
-- >   id: UUID!
-- >   created_at: timestampz!
-- >   errors: JSON
-- >   output: user_defined_type!
-- > }
actionAsyncQuery
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => ActionInfo 'Postgres
  -> m (Maybe (FieldParser n (AnnActionAsyncQuery 'Postgres UnpreparedValue)))
actionAsyncQuery actionInfo = runMaybeT do
  roleName <- lift askRoleName
  guard $ roleName == adminRoleName || roleName `Map.member` permissions
  actionId <- lift actionIdParser
  actionOutputParser <- lift $ actionOutputFields outputObject
  createdAtFieldParser <-
    lift $ P.column (ColumnScalar PGTimeStampTZ) (G.Nullability False)
  errorsFieldParser <-
    lift $ P.column (ColumnScalar PGJSON) (G.Nullability True)

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
  fmap P.mkParameter <$> P.column (ColumnScalar PGUUID) (G.Nullability False)

actionOutputFields
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m, Has QueryContext r)
  => AnnotatedObjectType 'Postgres
  -> m (Parser 'Output n (RQL.AnnFieldsG 'Postgres UnpreparedValue))
actionOutputFields outputObject = do
  let scalarOrEnumFields = map scalarOrEnumFieldParser $ toList $ _otdFields outputObject
  relationshipFields <- forM (_otdRelationships outputObject) $ traverse relationshipFieldParser
  let allFieldParsers = scalarOrEnumFields <>
                        maybe [] (catMaybes . toList) relationshipFields
      outputTypeName = unObjectTypeName $ _otdName outputObject
      outputTypeDescription = _otdDescription outputObject
  pure $ P.selectionSet outputTypeName outputTypeDescription allFieldParsers
         <&> parsedSelectionsToFields RQL.AFExpression
  where
    scalarOrEnumFieldParser
      :: ObjectFieldDefinition (G.GType, AnnotatedObjectFieldType)
      -> FieldParser n (RQL.AnnFieldG 'Postgres UnpreparedValue)
    scalarOrEnumFieldParser (ObjectFieldDefinition name _ description ty) =
      let (gType, objectFieldType) = ty
          fieldName = unObjectFieldName name
          -- FIXME? (from master)
          pgColumnInfo = ColumnInfo (unsafePGCol $ G.unName fieldName)
                         fieldName 0 (ColumnScalar PGJSON) (G.isNullable gType) Nothing
          fieldParser = case objectFieldType of
            AOFTScalar def -> customScalarParser def
            AOFTEnum def   -> customEnumParser def
      in bool P.nonNullableField id (G.isNullable gType) $
         P.selection_ (unObjectFieldName name) description fieldParser
         $> RQL.mkAnnColumnField pgColumnInfo Nothing

    relationshipFieldParser
      :: TypeRelationship (TableInfo 'Postgres) (ColumnInfo 'Postgres)
      -> m (Maybe (FieldParser n (RQL.AnnFieldG 'Postgres UnpreparedValue)))
    relationshipFieldParser typeRelationship = runMaybeT do
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
        in case relType of
              ObjRel -> RQL.AFObjectRelation $ RQL.AnnRelationSelectG tableRelName columnMapping $
                        RQL.AnnObjectSelectG (RQL._asnFields selectExp) tableName $
                        RQL._tpFilter $ RQL._asnPerm selectExp
              ArrRel -> RQL.AFArrayRelation $ RQL.ASSimple $
                        RQL.AnnRelationSelectG tableRelName columnMapping selectExp

mkDefinitionList :: AnnotatedObjectType 'Postgres -> [(PGCol, ScalarType 'Postgres)]
mkDefinitionList ObjectTypeDefinition{..} =
  flip map (toList _otdFields) $ \ObjectFieldDefinition{..} ->
    (unsafePGCol . G.unName . unObjectFieldName $ _ofdName,) $
    case Map.lookup _ofdName fieldReferences of
      Nothing         -> fieldTypeToScalarType $ snd _ofdType
      Just columnInfo -> unsafePGColumnToBackend $ pgiType columnInfo
  where
    fieldReferences =
      Map.unions $ map _trFieldMapping $ maybe [] toList _otdRelationships


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
      in Map.fromList . catMaybes <$> traverse mkTuple inputFields

    argumentParser
      :: G.Name
      -> Maybe G.Description
      -> G.GType
      -> NonObjectCustomType
      -> m (InputFieldsParser n (Maybe J.Value))
    argumentParser name description gType = \case
      NOCTScalar def -> pure $ mkArgumentInputFieldParser name description gType $ customScalarParser def
      NOCTEnum def -> pure $ mkArgumentInputFieldParser name description gType $ customEnumParser def
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

        pure $ mkArgumentInputFieldParser name description gType $
               P.object objectName objectDescription $
               J.Object <$> inputFieldsToObject inputFieldsParsers

mkArgumentInputFieldParser
  :: forall m k. (MonadParse m, 'Input P.<: k)
  => G.Name
  -> Maybe G.Description
  -> G.GType
  -> Parser k m J.Value
  -> InputFieldsParser m (Maybe J.Value)
mkArgumentInputFieldParser name description gType parser =
  if G.isNullable gType
  then P.fieldOptional name description modifiedParser
  else Just <$> P.field name description modifiedParser
  where
    modifiedParser = parserModifier gType parser

    parserModifier
      :: G.GType -> Parser k m J.Value -> Parser k m J.Value
    parserModifier = \case
      G.TypeNamed nullable _    -> nullableModifier nullable
      G.TypeList nullable ty ->
        nullableModifier nullable . fmap J.toJSON . P.list . parserModifier ty
      where
        nullableModifier =
          bool (fmap J.toJSON) (fmap J.toJSON . P.nullable) . G.unNullability

customScalarParser
  :: MonadParse m
  => AnnotatedScalarType -> Parser 'Both m J.Value
customScalarParser = \case
  ASTCustom ScalarTypeDefinition{..} ->
        if | _stdName == idScalar     -> J.toJSON <$> P.identifier
           | _stdName == intScalar    -> J.toJSON <$> P.int
           | _stdName == floatScalar  -> J.toJSON <$> P.float
           | _stdName == stringScalar -> J.toJSON <$> P.string
           | _stdName == boolScalar   -> J.toJSON <$> P.boolean
           | otherwise                -> P.namedJSON _stdName _stdDescription
  ASTReusedScalar name pgScalarType ->
    let schemaType = P.NonNullable $ P.TNamed $ P.mkDefinition name Nothing P.TIScalar
    in P.Parser
       { pType = schemaType
       , pParser = P.valueToJSON (P.toGraphQLType schemaType) >=>
                   either (parseErrorWith ParseFailed . qeError)
                   (pure . pgScalarValueToJson) . runAesonParser (parsePGValue pgScalarType)
       }

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
