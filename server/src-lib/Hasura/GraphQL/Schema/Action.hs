{-# LANGUAGE TemplateHaskellQuotes #-}

module Hasura.GraphQL.Schema.Action
  ( actionExecute,
    actionAsyncMutation,
    actionAsyncQuery,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.HashMap.Strict qualified as HashMap
import Data.Text.Extended
import Data.Text.NonEmpty
import Hasura.Backends.Postgres.Instances.Schema ()
import Hasura.Backends.Postgres.SQL.Types
import Hasura.Backends.Postgres.Types.Column
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (toErrorMessage)
import Hasura.GraphQL.Parser.Class
import Hasura.GraphQL.Parser.Internal.Scalars (mkScalar)
import Hasura.GraphQL.Parser.Name qualified as GName
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Parser
  ( FieldParser,
    InputFieldsParser,
    Kind (..),
    Parser,
  )
import Hasura.GraphQL.Schema.Parser qualified as P
import Hasura.Name qualified as Name
import Hasura.Prelude
import Hasura.RQL.IR.Action qualified as IR
import Hasura.RQL.IR.Root qualified as IR
import Hasura.RQL.IR.Value qualified as IR
import Hasura.RQL.Types.Action
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.CustomTypes
import Hasura.RQL.Types.NamingCase
import Hasura.RQL.Types.Relationships.Remote
import Hasura.RQL.Types.Roles (adminRoleName)
import Hasura.RQL.Types.Schema.Options qualified as Options
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

-- | actionExecute is used to execute either a query action or a synchronous
--   mutation action. A query action or a synchronous mutation action accepts
--   the field name and input arguments and a selectionset. The
--   input argument and selectionset types are defined by the user.
--
-- > action_name(action_input_arguments) {
-- >   col1: col1_type
-- >   col2: col2_type
-- > }
actionExecute ::
  forall r m n.
  (MonadBuildActionSchema r m n) =>
  AnnotatedCustomTypes ->
  ActionInfo ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnActionExecution (IR.RemoteRelationshipField IR.UnpreparedValue))))
actionExecute customTypes actionInfo = runMaybeT do
  roleName <- retrieve scRole
  guard (roleName == adminRoleName || roleName `HashMap.member` permissions)
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  inputArguments <- lift $ actionInputArguments (_actInputTypes customTypes) $ _adArguments definition
  parserOutput <- case outputObject of
    AOTObject aot -> do
      selectionSet <- lift $ actionOutputFields outputType aot (_actObjectTypes customTypes)
      pure $ P.subselection fieldName description inputArguments selectionSet
    AOTScalar ast -> do
      let selectionSet = customScalarParser ast
      pure $ P.selection fieldName description inputArguments selectionSet <&> (,[])
  pure
    $ parserOutput
    <&> \(argsJson, fields) ->
      IR.AnnActionExecution
        { _aaeName = actionName,
          _aaeFields = fields,
          _aaePayload = argsJson,
          _aaeOutputType = _adOutputType definition,
          _aaeOutputFields = IR.getActionOutputFields outputObject,
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
  (MonadBuildActionSchema r m n) =>
  HashMap G.Name AnnotatedInputType ->
  ActionInfo ->
  SchemaT r m (Maybe (FieldParser n IR.AnnActionMutationAsync))
actionAsyncMutation nonObjectTypeMap actionInfo = runMaybeT do
  roleName <- retrieve scRole
  guard $ roleName == adminRoleName || roleName `HashMap.member` permissions
  inputArguments <- lift $ actionInputArguments nonObjectTypeMap $ _adArguments definition
  let fieldName = unActionName actionName
      description = G.Description <$> comment
  pure
    $ P.selection fieldName description inputArguments actionIdParser
    <&> IR.AnnActionMutationAsync actionName forwardClientHeaders
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
  (MonadBuildActionSchema r m n) =>
  HashMap G.Name AnnotatedObjectType ->
  ActionInfo ->
  SchemaT r m (Maybe (FieldParser n (IR.AnnActionAsyncQuery ('Postgres 'Vanilla) (IR.RemoteRelationshipField IR.UnpreparedValue))))
actionAsyncQuery objectTypes actionInfo = runMaybeT do
  roleName <- retrieve scRole
  guard $ roleName == adminRoleName || roleName `HashMap.member` permissions
  createdAtFieldParser <- mkOutputParser PGTimeStampTZ
  errorsFieldParser <- P.nullable <$> mkOutputParser PGJSON
  let outputTypeName = unActionName actionName
      fieldName = unActionName actionName
      description = G.Description <$> comment
      actionIdInputField =
        P.field idFieldName (Just idFieldDescription) actionIdParser
      allFieldParsers actionOutputParser =
        let idField = P.selection_ idFieldName (Just idFieldDescription) actionIdParser $> IR.AsyncId
            createdAtField =
              P.selection_
                Name._created_at
                (Just "the time at which this action was created")
                createdAtFieldParser
                $> IR.AsyncCreatedAt
            errorsField =
              P.selection_
                Name._errors
                (Just "errors related to the invocation")
                errorsFieldParser
                $> IR.AsyncErrors
            outputField =
              P.subselection_
                Name._output
                (Just "the output fields of this action")
                actionOutputParser
                <&> IR.AsyncOutput
         in [idField, createdAtField, errorsField, outputField]
  parserOutput <- case outputObject of
    AOTObject aot -> do
      actionOutputParser <- lift $ actionOutputFields outputType aot objectTypes
      let desc = G.Description $ "fields of action: " <>> actionName
          selectionSet =
            -- Note: If we want support for Apollo Federation for Actions later,
            -- we'd need to add support for "key" directive here as well.
            P.selectionSet outputTypeName (Just desc) (allFieldParsers actionOutputParser)
              <&> parsedSelectionsToFields IR.AsyncTypename
      pure $ P.subselection fieldName description actionIdInputField selectionSet
    AOTScalar ast -> do
      let selectionSet = customScalarParser ast
      pure $ P.selection fieldName description actionIdInputField selectionSet <&> (,[])

  stringifyNumbers <- retrieve Options.soStringifyNumbers
  definitionsList <- lift $ mkDefinitionList outputObject
  pure
    $ parserOutput
    <&> \(idArg, fields) ->
      IR.AnnActionAsyncQuery
        { _aaaqName = actionName,
          _aaaqActionId = idArg,
          _aaaqOutputType = _adOutputType definition,
          _aaaqFields = fields,
          _aaaqDefinitionList = definitionsList,
          _aaaqStringifyNum = stringifyNumbers,
          _aaaqForwardClientHeaders = forwardClientHeaders,
          _aaaqSource = getActionSourceInfo outputObject
        }
  where
    -- For historical reasons, we use postgres-specific scalar names for two
    -- specific output fields. To avoid calling all the postgres schema
    -- machienry (especially since we are not currently associated with a given
    -- PG source), we manually craft the corresponding output parsers.
    --
    -- Since we know that those parsers are only used for output scalar fields,
    -- we don't care about their output value: they are not used to parse input
    -- values, nor do they have a selection set to process.
    mkOutputParser :: forall m'. (MonadError QErr m') => PGScalarType -> m' (Parser 'Both n ())
    mkOutputParser scalarType = do
      gName <- mkScalarTypeName HasuraCase scalarType
      pure $ mkScalar gName $ const $ pure ()

    ActionInfo actionName (outputType, outputObject) definition permissions forwardClientHeaders comment = actionInfo
    idFieldName = Name._id
    idFieldDescription = "the unique id of an action"

    getActionSourceInfo :: AnnotatedOutputType -> IR.ActionSourceInfo ('Postgres 'Vanilla)
    getActionSourceInfo = \case
      AOTObject aot -> fromMaybe IR.ASINoSource $ listToMaybe do
        AnnotatedTypeRelationship {..} <- _aotRelationships aot
        pure $ IR.ASISource _atrSource _atrSourceConfig
      AOTScalar _ -> IR.ASINoSource

    mkDefinitionList :: AnnotatedOutputType -> SchemaT r m [(PGCol, ScalarType ('Postgres 'Vanilla))]
    mkDefinitionList = \case
      AOTScalar _ -> pure []
      AOTObject AnnotatedObjectType {..} -> do
        let fieldReferences = HashMap.unions $ map _atrFieldMapping _aotRelationships
        for (toList _aotFields) \ObjectFieldDefinition {..} ->
          (unsafePGCol . G.unName . unObjectFieldName $ _ofdName,)
            <$> case HashMap.lookup _ofdName fieldReferences of
              Nothing -> fieldTypeToScalarType $ snd _ofdType
              Just columnInfo -> pure $ unsafePGColumnToBackend $ ciType columnInfo

    -- warning: we don't support other backends than Postgres for async queries;
    -- here, we fail if we encounter a non-Postgres scalar type
    fieldTypeToScalarType :: AnnotatedObjectFieldType -> SchemaT r m PGScalarType
    fieldTypeToScalarType = \case
      AOFTEnum _ -> pure PGText
      AOFTObject _ -> pure PGJSON
      AOFTScalar annotatedScalar -> case annotatedScalar of
        ASTReusedScalar _ scalar ->
          case AB.unpackAnyBackend @('Postgres 'Vanilla) scalar of
            Just pgScalar -> pure $ unwrapScalar pgScalar
            Nothing -> throw500 "encountered non-Postgres scalar in async query actions"
        ASTCustom ScalarTypeDefinition {..} ->
          pure
            $ if
              | _stdName == GName._ID -> PGText
              | _stdName == GName._Int -> PGInteger
              | _stdName == GName._Float -> PGFloat
              | _stdName == GName._String -> PGText
              | _stdName == GName._Boolean -> PGBoolean
              | otherwise -> PGJSON

-- | Async action's unique id
actionIdParser :: (MonadParse n) => Parser 'Both n ActionId
actionIdParser = ActionId <$> P.uuid

actionOutputFields ::
  forall r m n.
  (MonadBuildActionSchema r m n) =>
  G.GType ->
  AnnotatedObjectType ->
  HashMap G.Name AnnotatedObjectType ->
  SchemaT r m (Parser 'Output n (AnnotatedActionFields))
actionOutputFields outputType annotatedObject objectTypes = do
  scalarOrEnumOrObjectFields <- forM (toList $ _aotFields annotatedObject) outputFieldParser
  relationshipFields <- traverse relationshipFieldParser $ _aotRelationships annotatedObject
  let outputTypeName = unObjectTypeName $ _aotName annotatedObject
      allFieldParsers =
        scalarOrEnumOrObjectFields
          <> concat (catMaybes relationshipFields)
      outputTypeDescription = _aotDescription annotatedObject
  pure
    $ outputParserModifier outputType
    $ P.selectionSet outputTypeName outputTypeDescription allFieldParsers
    <&> parsedSelectionsToFields IR.ACFExpression
  where
    outputParserModifier :: G.GType -> Parser 'Output n a -> Parser 'Output n a
    outputParserModifier = \case
      G.TypeNamed (G.Nullability True) _ -> P.nullableParser
      G.TypeNamed (G.Nullability False) _ -> P.nonNullableParser
      G.TypeList (G.Nullability True) t -> P.nullableParser . P.multiple . outputParserModifier t
      G.TypeList (G.Nullability False) t -> P.nonNullableParser . P.multiple . outputParserModifier t

    outputFieldParser ::
      ObjectFieldDefinition (G.GType, AnnotatedObjectFieldType) ->
      SchemaT r m (FieldParser n (AnnotatedActionField))
    outputFieldParser (ObjectFieldDefinition name _ description (gType, objectFieldType)) = P.memoizeOn 'actionOutputFields (_aotName annotatedObject, name) do
      case objectFieldType of
        AOFTScalar def ->
          wrapScalar $ customScalarParser def
        AOFTEnum def ->
          wrapScalar $ customEnumParser def
        AOFTObject objectName -> do
          def <- HashMap.lookup objectName objectTypes `onNothing` throw500 ("Custom type " <> objectName <<> " not found")
          parser <- fmap (IR.ACFNestedObject fieldName) <$> actionOutputFields gType def objectTypes
          pure $ P.subselection_ fieldName description parser
      where
        fieldName = unObjectFieldName name
        wrapScalar parser =
          pure
            $ P.wrapFieldParser gType (P.selection_ fieldName description parser)
            $> IR.ACFScalar fieldName

    relationshipFieldParser ::
      AnnotatedTypeRelationship ->
      SchemaT r m (Maybe [FieldParser n (AnnotatedActionField)])
    relationshipFieldParser (AnnotatedTypeRelationship {..}) = runMaybeT do
      relName <- hoistMaybe $ RelName <$> mkNonEmptyText (toTxt _atrName)

      --  `lhsJoinFields` is a map of `x: y`
      --  where 'x' is the 'reference name' of a join field, i.e, how a join
      --         field is referenced in the remote relationships definition
      --  while 'y' is the join field.
      --  In case of logical models, they are pretty much the same.
      --  In case of databases, 'y' could be a computed field with session variables etc.
      let lhsJoinFields = HashMap.fromList [(FieldName $ G.unName k, k) | ObjectFieldName k <- HashMap.keys _atrFieldMapping]
          joinMapping = HashMap.fromList $ do
            (k, v) <- HashMap.toList _atrFieldMapping
            let scalarType = case ciType v of
                  ColumnScalar scalar -> scalar
                  -- We don't currently allow enum types as fields of logical models so they should not appear here.
                  -- If we do allow them in future then they would be represented in Postgres as Text.
                  ColumnEnumReference _ -> PGText
            pure (FieldName $ G.unName $ unObjectFieldName k, (scalarType, ciColumn v))
          remoteFieldInfo =
            RemoteFieldInfo
              { _rfiLHS = lhsJoinFields,
                _rfiRHS =
                  RFISource
                    $ AB.mkAnyBackend @('Postgres 'Vanilla)
                    $ RemoteSourceFieldInfo
                      { _rsfiName = relName,
                        _rsfiType = _atrType,
                        _rsfiSource = _atrSource,
                        _rsfiSourceConfig = _atrSourceConfig,
                        _rsfiTable = _atrTableName,
                        _rsfiMapping = joinMapping
                      }
              }
      RemoteRelationshipParserBuilder remoteRelationshipField <- retrieve scRemoteRelationshipParserBuilder
      remoteRelationshipFieldParsers <- MaybeT $ remoteRelationshipField remoteFieldInfo
      pure $ remoteRelationshipFieldParsers <&> fmap (IR.ACFRemote . IR.ActionRemoteRelationshipSelect lhsJoinFields)

actionInputArguments ::
  forall r m n.
  (MonadBuildActionSchema r m n) =>
  HashMap G.Name AnnotatedInputType ->
  [ArgumentDefinition (G.GType, AnnotatedInputType)] ->
  SchemaT r m (InputFieldsParser n J.Value)
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
      let mkTuple (name, parser) = fmap (K.fromText (G.unName name),) <$> parser
       in KM.fromList . catMaybes <$> traverse mkTuple inputFields

    argumentParser ::
      G.Name ->
      Maybe G.Description ->
      G.GType ->
      AnnotatedInputType ->
      SchemaT r m (InputFieldsParser n (Maybe J.Value))
    argumentParser name description gType nonObjectType = do
      let mkResult :: forall k. ('Input P.<: k) => Parser k n J.Value -> InputFieldsParser n (Maybe J.Value)
          mkResult = mkArgumentInputFieldParser name description gType
      case nonObjectType of
        -- scalar and enum parsers are not recursive and need not be memoized
        NOCTScalar def -> pure $ mkResult $ customScalarParser def
        NOCTEnum def -> pure $ mkResult $ customEnumParser def
        -- input objects however may recursively contain one another
        NOCTInputObject (InputObjectTypeDefinition (InputObjectTypeName objectName) objectDesc inputFields) ->
          mkResult <$> P.memoizeOn 'actionInputArguments objectName do
            inputFieldsParsers <- forM
              (toList inputFields)
              \(InputObjectFieldDefinition (InputObjectFieldName fieldName) fieldDesc (GraphQLType fieldType)) -> do
                nonObjectFieldType <-
                  HashMap.lookup (G.getBaseType fieldType) nonObjectTypeMap
                    `onNothing` throw500 "object type for a field found in custom input object type"
                (fieldName,) <$> argumentParser fieldName fieldDesc fieldType nonObjectFieldType
            pure
              $ P.object objectName objectDesc
              $ J.Object
              <$> inputFieldsToObject inputFieldsParsers

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
  (MonadParse m) =>
  AnnotatedScalarType ->
  Parser 'Both m J.Value
customScalarParser = \case
  ASTCustom ScalarTypeDefinition {..} ->
    if
      | _stdName == GName._ID -> J.toJSON <$> P.identifier
      | _stdName == GName._Int -> J.toJSON <$> P.int
      | _stdName == GName._Float -> J.toJSON <$> P.float
      | _stdName == GName._String -> J.toJSON <$> P.string
      | _stdName == GName._Boolean -> J.toJSON <$> P.boolean
      | otherwise -> P.jsonScalar _stdName _stdDescription
  ASTReusedScalar name backendScalarType ->
    let schemaType = P.TNamed P.NonNullable $ P.Definition name Nothing Nothing [] P.TIScalar
        backendScalarValidator =
          AB.dispatchAnyBackend @Backend backendScalarType \(scalarType :: ScalarWrapper b) jsonInput -> do
            -- We attempt to parse the value from JSON to validate it, but still
            -- output it as JSON. On one hand this allows us to detect issues
            -- ahead of time: if the value is not formatted correctly, we don't
            -- send the action at all; on the other, it means we are at risk of
            -- rejecting valid queries if our parser is more strict than the one
            -- of the remote server. We do not parse scalars for remote servers
            -- for that reason; we might want to reconsider this validation as
            -- well.
            void
              $ parseScalarValue @b (parsingContext scalarType) (unwrapScalar scalarType) jsonInput
              `onLeft` \e -> parseErrorWith P.ParseFailed . toErrorMessage $ qeError e
            pure jsonInput
     in P.Parser
          { pType = schemaType,
            pParser = P.valueToJSON (P.toGraphQLType schemaType) >=> backendScalarValidator
          }

customEnumParser ::
  (MonadParse m) =>
  EnumTypeDefinition ->
  Parser 'Both m J.Value
customEnumParser (EnumTypeDefinition typeName description enumValues) =
  let enumName = unEnumTypeName typeName
      enumValueDefinitions =
        enumValues <&> \enumValue ->
          let valueName = G.unEnumValue $ _evdValue enumValue
           in (,J.toJSON valueName)
                $ P.Definition
                  valueName
                  (_evdDescription enumValue)
                  Nothing
                  []
                  P.EnumValueInfo
   in P.enum enumName description enumValueDefinitions
