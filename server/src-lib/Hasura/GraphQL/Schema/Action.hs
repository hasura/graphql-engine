module Hasura.GraphQL.Schema.Action
  ( actionExecute
  , actionAsyncMutation
  , actionAsyncQuery
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.HashMap.Strict                   as Map
import qualified Language.GraphQL.Draft.Syntax         as G

import           Data.Has
import           Data.Text.Extended
import           Data.Text.NonEmpty

import qualified Hasura.GraphQL.Parser                 as P
import qualified Hasura.GraphQL.Parser.Internal.Parser as P
import qualified Hasura.RQL.DML.Internal               as RQL
import qualified Hasura.RQL.IR.Select                  as RQL

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Backends.Postgres.Types.Column
import           Hasura.GraphQL.Parser                 (FieldParser, InputFieldsParser, Kind (..),
                                                        Parser, UnpreparedValue (..))
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Backend
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
  :: forall m n r
   . ( BackendSchema 'Postgres
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => NonObjectTypeMap
  -> ActionInfo
  -> m (Maybe (FieldParser n (AnnActionExecution 'Postgres (UnpreparedValue 'Postgres))))
actionExecute nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- askRoleName
  guard (roleName == adminRoleName || roleName `Map.member` permissions)
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  selectionSet <- lift $ actionOutputFields outputType outputObject
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
               , _aaeSource  = getActionSourceInfo outputObject
               }
  where
    ActionInfo actionName (outputType, outputObject) definition permissions _ comment = actionInfo

-- | actionAsyncMutation is used to execute a asynchronous mutation action. An
--   asynchronous action expects the field name and the input arguments to the
--   action. A selectionset is *not* expected. An action ID (UUID) will be
--   returned after performing the action
--
-- > action_name(action_input_arguments)
actionAsyncMutation
  :: forall m n r. (MonadSchema n m, MonadTableInfo r m, MonadRole r m)
  => NonObjectTypeMap
  -> ActionInfo
  -> m (Maybe (FieldParser n AnnActionMutationAsync))
actionAsyncMutation nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- lift askRoleName
  guard $ roleName == adminRoleName || roleName `Map.member` permissions
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  pure $ P.selection fieldName description inputArguments actionIdParser
    <&> AnnActionMutationAsync actionName forwardClientHeaders
  where
    ActionInfo actionName _ definition permissions forwardClientHeaders comment = actionInfo

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
  :: forall m n r
   . ( BackendSchema 'Postgres
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => ActionInfo
  -> m (Maybe (FieldParser n (AnnActionAsyncQuery 'Postgres (UnpreparedValue 'Postgres))))
actionAsyncQuery actionInfo = runMaybeT do
  roleName <- askRoleName
  guard $ roleName == adminRoleName || roleName `Map.member` permissions
  actionOutputParser <- lift $ actionOutputFields outputType outputObject
  createdAtFieldParser <-
    lift $ columnParser @'Postgres (ColumnScalar PGTimeStampTZ) (G.Nullability False)
  errorsFieldParser <-
    lift $ columnParser @'Postgres (ColumnScalar PGJSON) (G.Nullability True)

  let fieldName = unActionName actionName
      description = G.Description <$> comment
      actionIdInputField =
        P.field idFieldName (Just idFieldDescription) actionIdParser
      allFieldParsers =
        let idField        = P.selection_ idFieldName (Just idFieldDescription) actionIdParser $> AsyncId
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
              , _aaaqForwardClientHeaders = forwardClientHeaders
              , _aaaqSource  = getActionSourceInfo outputObject
              }
  where
    ActionInfo actionName (outputType, outputObject) definition permissions forwardClientHeaders comment = actionInfo
    idFieldName = $$(G.litName "id")
    idFieldDescription = "the unique id of an action"

-- | Async action's unique id
actionIdParser
  :: MonadParse n => Parser 'Both n ActionId
actionIdParser = ActionId <$> P.uuid

actionOutputFields
  :: forall m n r
   . ( BackendSchema 'Postgres
     , MonadSchema n m
     , MonadTableInfo r m
     , MonadRole r m
     , Has QueryContext r
     , Has (BackendExtension 'Postgres) r
     )
  => G.GType
  -> AnnotatedObjectType
  -> m (Parser 'Output n (RQL.AnnFieldsG 'Postgres (UnpreparedValue 'Postgres)))
actionOutputFields outputType annotatedObject = do
  let outputObject = _aotDefinition annotatedObject
      scalarOrEnumFields = map scalarOrEnumFieldParser $ toList $ _otdFields outputObject
  relationshipFields <- forM (_otdRelationships outputObject) $ traverse relationshipFieldParser
  let allFieldParsers = scalarOrEnumFields <>
                        maybe [] (concat . catMaybes . toList) relationshipFields
      outputTypeName = unObjectTypeName $ _otdName outputObject
      outputTypeDescription = _otdDescription outputObject
  pure $ mkOutputParserModifier outputType $
    P.selectionSet outputTypeName outputTypeDescription allFieldParsers
    <&> parsedSelectionsToFields RQL.AFExpression
  where
    scalarOrEnumFieldParser
      :: ObjectFieldDefinition (G.GType, AnnotatedObjectFieldType)
      -> FieldParser n (RQL.AnnFieldG 'Postgres (UnpreparedValue 'Postgres))
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
         $> RQL.mkAnnColumnField pgColumnInfo Nothing Nothing

    relationshipFieldParser
      :: TypeRelationship (TableInfo 'Postgres) (ColumnInfo 'Postgres)
      -> m (Maybe [FieldParser n (RQL.AnnFieldG 'Postgres (UnpreparedValue 'Postgres))])
    relationshipFieldParser (TypeRelationship relName relType _ tableInfo fieldMapping) = runMaybeT do
      let tableName     = _tciName $ _tiCoreInfo tableInfo
          fieldName     = unRelationshipName relName
          tableRelName  = RelName $ mkNonEmptyTextUnsafe $ G.unName fieldName
          columnMapping = Map.fromList $ do
            (k, v) <- Map.toList fieldMapping
            pure (unsafePGCol $ G.unName $ unObjectFieldName k, pgiColumn v)
      roleName   <- lift askRoleName
      tablePerms <- hoistMaybe $ RQL.getPermInfoMaybe roleName PASelect tableInfo
      case relType of
        ObjRel -> do
          let desc = Just $ G.Description "An object relationship"
          selectionSetParser <- lift $ tableSelectionSet tableName tablePerms
          pure $ pure $ P.nonNullableField $
            P.subselection_ fieldName desc selectionSetParser
              <&> \fields -> RQL.AFObjectRelation $ RQL.AnnRelationSelectG tableRelName columnMapping $
                             RQL.AnnObjectSelectG fields tableName $
                             fmapAnnBoolExp partialSQLExpToUnpreparedValue $ spiFilter tablePerms
        ArrRel -> do
          let desc = Just $ G.Description "An array relationship"
          otherTableParser <- lift $ selectTable tableName fieldName desc tablePerms
          let arrayRelField = otherTableParser <&> \selectExp -> RQL.AFArrayRelation $
                RQL.ASSimple $ RQL.AnnRelationSelectG tableRelName columnMapping selectExp
              relAggFieldName = fieldName <> $$(G.litName "_aggregate")
              relAggDesc      = Just $ G.Description "An aggregate relationship"
          tableAggField <- lift $ selectTableAggregate tableName relAggFieldName relAggDesc tablePerms
          pure $ catMaybes [ Just arrayRelField
                           , fmap (RQL.AFArrayRelation . RQL.ASAggregate . RQL.AnnRelationSelectG tableRelName columnMapping) <$> tableAggField
                           ]

mkOutputParserModifier :: G.GType -> Parser 'Output m a -> Parser 'Output m a
mkOutputParserModifier = \case
  G.TypeNamed nullable _ -> nullableModifier nullable
  G.TypeList nullable ty ->
    nullableModifier nullable . P.multiple . mkOutputParserModifier ty
  where
    nullableModifier nullable =
      if G.unNullability nullable then P.nullableParser else P.nonNullableParser

mkDefinitionList :: AnnotatedObjectType -> [(PGCol, ScalarType 'Postgres)]
mkDefinitionList AnnotatedObjectType{..} =
  flip map (toList _otdFields) $ \ObjectFieldDefinition{..} ->
    (unsafePGCol . G.unName . unObjectFieldName $ _ofdName,) $
    case Map.lookup _ofdName fieldReferences of
      Nothing         -> fieldTypeToScalarType $ snd _ofdType
      Just columnInfo -> unsafePGColumnToBackend $ pgiType columnInfo
  where
    ObjectTypeDefinition{..} = _aotDefinition
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
    argumentParser name description gType nonObjectType = do
      let mkResult :: forall k. ('Input P.<: k) => Parser k n J.Value -> InputFieldsParser n (Maybe J.Value)
          mkResult = mkArgumentInputFieldParser name description gType
      case nonObjectType of
        -- scalar and enum parsers are not recursive and need not be memoized
        NOCTScalar def -> pure $ mkResult $ customScalarParser def
        NOCTEnum   def -> pure $ mkResult $ customEnumParser   def
        -- input objects however may recursively contain one another
        NOCTInputObject (InputObjectTypeDefinition (InputObjectTypeName objectName) objectDesc inputFields) ->
          mkResult <$> memoizeOn 'actionInputArguments objectName do
            inputFieldsParsers <- forM (toList inputFields)
              \(InputObjectFieldDefinition (InputObjectFieldName fieldName) fieldDesc (GraphQLType fieldType)) -> do
                nonObjectFieldType <-
                  Map.lookup (G.getBaseType fieldType) nonObjectTypeMap
                  `onNothing` throw500 "object type for a field found in custom input object type"
                (fieldName,) <$> argumentParser fieldName fieldDesc fieldType nonObjectFieldType
            pure
              $ P.object objectName objectDesc
              $ J.Object <$> inputFieldsToObject inputFieldsParsers


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
                   (pure . scalarValueToJSON) . parseScalarValue pgScalarType
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
