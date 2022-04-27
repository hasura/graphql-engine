{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasura.GraphQL.Schema.Action
  ( actionExecute,
    actionAsyncMutation,
    actionAsyncQuery,
  )
where

import Data.Aeson qualified as J
import Data.Has
import Data.HashMap.Strict qualified as Map
import Data.Text.Extended
import Data.Text.NonEmpty
import Hasura.Backends.Postgres.Instances.Schema ()
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Types.Column
import Hasura.Base.Error
import Hasura.GraphQL.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    Parser,
    UnpreparedValue (..),
  )
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Parser.Internal.Parser qualified as P
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Select
import Hasura.Prelude
import Hasura.RQL.IR.Action qualified as RQL
import Hasura.RQL.IR.Root qualified as RQL
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Session
import Language.GraphQL.Draft.Syntax qualified as G

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
actionExecute ::
  forall r m n.
  MonadBuildSchema ('Postgres 'Vanilla) r m n =>
  AnnotatedCustomTypes ->
  ActionInfo ->
  m (Maybe (FieldParser n (AnnActionExecution (RQL.RemoteRelationshipField UnpreparedValue))))
actionExecute customTypes actionInfo = runMaybeT do
  roleName <- asks getter
  guard (roleName == adminRoleName || roleName `Map.member` permissions)
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  inputArguments <- lift $ actionInputArguments (_actNonObjects customTypes) $ _adArguments definition
  parserOutput <- case outputObject of
    AOTObject aot -> do
      selectionSet <- lift $ actionOutputFields outputType aot (_actObjects customTypes)
      pure $ P.subselection fieldName description inputArguments selectionSet
    AOTScalar ast -> do
      let selectionSet = customScalarParser ast
      pure $ P.selection fieldName description inputArguments selectionSet <&> (,[])
  pure $
    parserOutput
      <&> \(argsJson, fields) ->
        AnnActionExecution
          { _aaeName = actionName,
            _aaeFields = fields,
            _aaePayload = argsJson,
            _aaeOutputType = _adOutputType definition,
            _aaeOutputFields = getActionOutputFields outputObject,
            _aaeWebhook = _adHandler definition,
            _aaeHeaders = _adHeaders definition,
            _aaeForwardClientHeaders = _adForwardClientHeaders definition,
            _aaeTimeOut = _adTimeout definition,
            _aaeRequestTransform = _adRequestTransform definition,
            _aaeResponseTransform = _adResponseTransform definition
          }
  where
    ActionInfo actionName (outputType, outputObject) definition permissions _ comment = actionInfo

-- | actionAsyncMutation is used to execute a asynchronous mutation action. An
--   asynchronous action expects the field name and the input arguments to the
--   action. A selectionset is *not* expected. An action ID (UUID) will be
--   returned after performing the action
--
-- > action_name(action_input_arguments)
actionAsyncMutation ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  NonObjectTypeMap ->
  ActionInfo ->
  m (Maybe (FieldParser n AnnActionMutationAsync))
actionAsyncMutation nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- asks getter
  guard $ roleName == adminRoleName || roleName `Map.member` permissions
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  pure $
    P.selection fieldName description inputArguments actionIdParser
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
actionAsyncQuery ::
  forall r m n.
  MonadBuildSchema ('Postgres 'Vanilla) r m n =>
  AnnotatedObjects ->
  ActionInfo ->
  m (Maybe (FieldParser n (AnnActionAsyncQuery ('Postgres 'Vanilla) (RQL.RemoteRelationshipField UnpreparedValue))))
actionAsyncQuery objectTypes actionInfo = runMaybeT do
  roleName <- asks getter
  guard $ roleName == adminRoleName || roleName `Map.member` permissions
  createdAtFieldParser <-
    lift $ columnParser @('Postgres 'Vanilla) (ColumnScalar PGTimeStampTZ) (G.Nullability False)
  errorsFieldParser <-
    lift $ columnParser @('Postgres 'Vanilla) (ColumnScalar PGJSON) (G.Nullability True)

  outputTypeName <- P.mkTypename $ unActionName actionName
  let fieldName = unActionName actionName
      description = G.Description <$> comment
      actionIdInputField =
        P.field idFieldName (Just idFieldDescription) actionIdParser
      allFieldParsers actionOutputParser =
        let idField = P.selection_ idFieldName (Just idFieldDescription) actionIdParser $> AsyncId
            createdAtField =
              P.selection_
                G._created_at
                (Just "the time at which this action was created")
                createdAtFieldParser
                $> AsyncCreatedAt
            errorsField =
              P.selection_
                G._errors
                (Just "errors related to the invocation")
                errorsFieldParser
                $> AsyncErrors
            outputField =
              P.subselection_
                G._output
                (Just "the output fields of this action")
                actionOutputParser
                <&> AsyncOutput
         in [idField, createdAtField, errorsField, outputField]
  parserOutput <- case outputObject of
    AOTObject aot -> do
      actionOutputParser <- lift $ actionOutputFields outputType aot objectTypes
      let desc = G.Description $ "fields of action: " <>> actionName
          selectionSet =
            P.selectionSet outputTypeName (Just desc) (allFieldParsers actionOutputParser)
              <&> parsedSelectionsToFields AsyncTypename
      pure $ P.subselection fieldName description actionIdInputField selectionSet
    AOTScalar ast -> do
      let selectionSet = customScalarParser ast
      pure $ P.selection fieldName description actionIdInputField selectionSet <&> (,[])

  stringifyNum <- asks $ qcStringifyNum . getter
  pure $
    parserOutput
      <&> \(idArg, fields) ->
        AnnActionAsyncQuery
          { _aaaqName = actionName,
            _aaaqActionId = idArg,
            _aaaqOutputType = _adOutputType definition,
            _aaaqFields = fields,
            _aaaqDefinitionList = mkDefinitionList outputObject,
            _aaaqStringifyNum = stringifyNum,
            _aaaqForwardClientHeaders = forwardClientHeaders,
            _aaaqSource = getActionSourceInfo outputObject
          }
  where
    ActionInfo actionName (outputType, outputObject) definition permissions forwardClientHeaders comment = actionInfo
    idFieldName = G._id
    idFieldDescription = "the unique id of an action"

-- | Async action's unique id
actionIdParser ::
  MonadParse n => Parser 'Both n ActionId
actionIdParser = ActionId <$> P.uuid

actionOutputFields ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  G.GType ->
  AnnotatedObjectType ->
  AnnotatedObjects ->
  m (Parser 'Output n (AnnotatedActionFields))
actionOutputFields outputType annotatedObject objectTypes = do
  let outputObject = _aotDefinition annotatedObject
  scalarOrEnumOrObjectFields <- forM (toList $ _otdFields outputObject) outputFieldParser
  relationshipFields <- forM (_otdRelationships outputObject) $ traverse relationshipFieldParser
  outputTypeName <- P.mkTypename $ unObjectTypeName $ _otdName outputObject
  let allFieldParsers =
        scalarOrEnumOrObjectFields
          <> maybe [] (concat . catMaybes . toList) relationshipFields
      outputTypeDescription = _otdDescription outputObject
  pure $
    outputParserModifier outputType $
      P.selectionSet outputTypeName outputTypeDescription allFieldParsers
        <&> parsedSelectionsToFields RQL.ACFExpression
  where
    outputParserModifier :: G.GType -> Parser 'Output n a -> Parser 'Output n a
    outputParserModifier = \case
      G.TypeNamed (G.Nullability True) _ -> P.nullableParser
      G.TypeNamed (G.Nullability False) _ -> P.nonNullableParser
      G.TypeList (G.Nullability True) t -> P.nullableParser . P.multiple . outputParserModifier t
      G.TypeList (G.Nullability False) t -> P.nonNullableParser . P.multiple . outputParserModifier t

    outputFieldParser ::
      ObjectFieldDefinition (G.GType, AnnotatedObjectFieldType) ->
      m (FieldParser n (AnnotatedActionField))
    outputFieldParser (ObjectFieldDefinition name _ description (gType, objectFieldType)) = memoizeOn 'actionOutputFields (_otdName $ _aotDefinition annotatedObject, name) do
      case objectFieldType of
        AOFTScalar def ->
          wrapScalar $ customScalarParser def
        AOFTEnum def ->
          wrapScalar $ customEnumParser def
        AOFTObject objectName -> do
          def <- Map.lookup objectName objectTypes `onNothing` throw500 ("Custom type " <> objectName <<> " not found")
          parser <- fmap (RQL.ACFNestedObject fieldName) <$> actionOutputFields gType def objectTypes
          pure $ P.subselection_ fieldName description parser
      where
        fieldName = unObjectFieldName name
        wrapScalar parser =
          pure $
            P.wrapFieldParser gType (P.selection_ fieldName description parser)
              $> RQL.ACFScalar fieldName

    relationshipFieldParser ::
      TypeRelationship (TableInfo ('Postgres 'Vanilla)) (ColumnInfo ('Postgres 'Vanilla)) ->
      m (Maybe [FieldParser n (AnnotatedActionField)])
    relationshipFieldParser (TypeRelationship relationshipName relType sourceName tableInfo fieldMapping) = runMaybeT do
      sourceInfo <- MaybeT $ asks $ (unsafeSourceInfo @('Postgres 'Vanilla) <=< Map.lookup sourceName) . getter
      relName <- hoistMaybe $ RelName <$> mkNonEmptyText (toTxt relationshipName)

      --  `lhsJoinFields` is a map of `x: y`
      --  where 'x' is the 'reference name' of a join field, i.e, how a join
      --         field is referenced in the remote relationships definition
      --  while 'y' is the join field.
      --  In case of custom types, they are pretty much the same.
      --  In case of databases, 'y' could be a computed field with session variables etc.
      let lhsJoinFields = Map.fromList [(FieldName $ G.unName k, k) | ObjectFieldName k <- Map.keys fieldMapping]
          joinMapping = Map.fromList $ do
            (k, v) <- Map.toList fieldMapping
            let scalarType = case ciType v of
                  ColumnScalar scalar -> scalar
                  -- We don't currently allow enum types as fields of custom types so they should not appear here.
                  -- If we do allow them in future then they would be represented in Postgres as Text.
                  ColumnEnumReference _ -> PGText
            pure (FieldName $ G.unName $ unObjectFieldName k, (scalarType, ciColumn v))
          remoteFieldInfo =
            RemoteFieldInfo
              { _rfiLHS = lhsJoinFields,
                _rfiRHS =
                  RFISource $
                    AB.mkAnyBackend @('Postgres 'Vanilla) $
                      RemoteSourceFieldInfo
                        { _rsfiName = relName,
                          _rsfiType = relType,
                          _rsfiSource = sourceName,
                          _rsfiSourceConfig = _siConfiguration sourceInfo,
                          _rsfiSourceCustomization = getSourceTypeCustomization $ _siCustomization sourceInfo,
                          _rsfiTable = tableInfoName tableInfo,
                          _rsfiMapping = joinMapping
                        }
              }
      remoteRelationshipFieldParsers <- MaybeT $ remoteRelationshipField remoteFieldInfo
      pure $ remoteRelationshipFieldParsers <&> fmap (RQL.ACFRemote . RQL.ActionRemoteRelationshipSelect lhsJoinFields)

mkDefinitionList :: AnnotatedOutputType -> [(PGCol, ScalarType ('Postgres 'Vanilla))]
mkDefinitionList (AOTScalar _) = []
mkDefinitionList (AOTObject AnnotatedObjectType {..}) =
  flip map (toList _otdFields) $ \ObjectFieldDefinition {..} ->
    (unsafePGCol . G.unName . unObjectFieldName $ _ofdName,) $
      case Map.lookup _ofdName fieldReferences of
        Nothing -> fieldTypeToScalarType $ snd _ofdType
        Just columnInfo -> unsafePGColumnToBackend $ ciType columnInfo
  where
    ObjectTypeDefinition {..} = _aotDefinition
    fieldReferences =
      Map.unions $ map _trFieldMapping $ maybe [] toList _otdRelationships

actionInputArguments ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  NonObjectTypeMap ->
  [ArgumentDefinition (G.GType, NonObjectCustomType)] ->
  m (InputFieldsParser n J.Value)
actionInputArguments nonObjectTypeMap arguments = do
  argumentParsers <- for arguments $ \argument -> do
    let ArgumentDefinition argumentName (gType, nonObjectType) argumentDescription = argument
        name = unArgumentName argumentName
    (name,) <$> argumentParser name argumentDescription gType nonObjectType
  pure $ J.Object <$> inputFieldsToObject argumentParsers
  where
    inputFieldsToObject ::
      [(G.Name, InputFieldsParser n (Maybe J.Value))] ->
      InputFieldsParser n J.Object
    inputFieldsToObject inputFields =
      let mkTuple (name, parser) = fmap (G.unName name,) <$> parser
       in Map.fromList . catMaybes <$> traverse mkTuple inputFields

    argumentParser ::
      G.Name ->
      Maybe G.Description ->
      G.GType ->
      NonObjectCustomType ->
      m (InputFieldsParser n (Maybe J.Value))
    argumentParser name description gType nonObjectType = do
      let mkResult :: forall k. ('Input P.<: k) => Parser k n J.Value -> InputFieldsParser n (Maybe J.Value)
          mkResult = mkArgumentInputFieldParser name description gType
      case nonObjectType of
        -- scalar and enum parsers are not recursive and need not be memoized
        NOCTScalar def -> pure $ mkResult $ customScalarParser def
        NOCTEnum def -> pure $ mkResult $ customEnumParser def
        -- input objects however may recursively contain one another
        NOCTInputObject (InputObjectTypeDefinition (InputObjectTypeName objectName) objectDesc inputFields) ->
          mkResult <$> memoizeOn 'actionInputArguments objectName do
            inputFieldsParsers <- forM
              (toList inputFields)
              \(InputObjectFieldDefinition (InputObjectFieldName fieldName) fieldDesc (GraphQLType fieldType)) -> do
                nonObjectFieldType <-
                  Map.lookup (G.getBaseType fieldType) nonObjectTypeMap
                    `onNothing` throw500 "object type for a field found in custom input object type"
                (fieldName,) <$> argumentParser fieldName fieldDesc fieldType nonObjectFieldType
            pure $
              P.object objectName objectDesc $
                J.Object <$> inputFieldsToObject inputFieldsParsers

mkArgumentInputFieldParser ::
  forall m k.
  (MonadParse m, 'Input P.<: k) =>
  G.Name ->
  Maybe G.Description ->
  G.GType ->
  Parser k m J.Value ->
  InputFieldsParser m (Maybe J.Value)
mkArgumentInputFieldParser name description gType parser =
  if G.isNullable gType
    then P.fieldOptional name description modifiedParser
    else Just <$> P.field name description modifiedParser
  where
    modifiedParser = parserModifier gType parser

    parserModifier ::
      G.GType -> Parser k m J.Value -> Parser k m J.Value
    parserModifier = \case
      G.TypeNamed nullable _ -> nullableModifier nullable
      G.TypeList nullable ty ->
        nullableModifier nullable . fmap J.toJSON . P.list . parserModifier ty
      where
        nullableModifier =
          bool (fmap J.toJSON) (fmap J.toJSON . P.nullable) . G.unNullability

customScalarParser ::
  MonadParse m =>
  AnnotatedScalarType ->
  Parser 'Both m J.Value
customScalarParser = \case
  ASTCustom ScalarTypeDefinition {..} ->
    if
        | _stdName == idScalar -> J.toJSON <$> P.identifier
        | _stdName == intScalar -> J.toJSON <$> P.int
        | _stdName == floatScalar -> J.toJSON <$> P.float
        | _stdName == stringScalar -> J.toJSON <$> P.string
        | _stdName == boolScalar -> J.toJSON <$> P.boolean
        | otherwise -> P.jsonScalar _stdName _stdDescription
  ASTReusedScalar name pgScalarType ->
    let schemaType = P.TNamed P.NonNullable $ P.Definition name Nothing P.TIScalar
     in P.Parser
          { pType = schemaType,
            pParser =
              P.valueToJSON (P.toGraphQLType schemaType)
                >=> either
                  (parseErrorWith ParseFailed . qeError)
                  (pure . scalarValueToJSON @('Postgres 'Vanilla))
                  . parseScalarValue @('Postgres 'Vanilla) pgScalarType
          }

customEnumParser ::
  MonadParse m =>
  EnumTypeDefinition ->
  Parser 'Both m J.Value
customEnumParser (EnumTypeDefinition typeName description enumValues) =
  let enumName = unEnumTypeName typeName
      enumValueDefinitions =
        enumValues <&> \enumValue ->
          let valueName = G.unEnumValue $ _evdValue enumValue
           in (,J.toJSON valueName) $
                P.Definition
                  valueName
                  (_evdDescription enumValue)
                  P.EnumValueInfo
   in P.enum enumName description enumValueDefinitions
