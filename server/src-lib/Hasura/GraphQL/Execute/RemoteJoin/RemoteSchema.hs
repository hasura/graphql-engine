-- | How to construct and execute a call to a remote schema for a remote join.
--
-- There are three steps required to do this:
--   1. construct the call: given the requested fields, the phantom fields, the
--      values extracted by the LHS, construct a GraphQL query
--   2. execute that GraphQL query over the network
--   3. build a index of the variables out of the response
--
-- This can be done as one function, but we also export the individual steps for
-- debugging / test purposes. We congregate all intermediary state in the opaque
-- 'RemoteSchemaCall' type.
module Hasura.GraphQL.Execute.RemoteJoin.RemoteSchema
  ( -- * Executing a remote join
    makeRemoteSchemaJoinCall,

    -- * Individual steps
    RemoteSchemaCall,
    buildRemoteSchemaCall,
    executeRemoteSchemaCall,
    buildJoinIndex,
  )
where

import Control.Lens (view, _2, _3)
import Data.Aeson qualified as J
import Data.Aeson.Ordered qualified as AO
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated, toTxt, (<<>), (<>>))
import Data.Validation (Validation (..), toEither)
import Hasura.Base.Error
import Hasura.Base.ErrorMessage (fromErrorMessage)
import Hasura.GraphQL.Execute.Remote
  ( getVariableDefinitionAndValue,
    resolveRemoteVariable,
    runVariableCache,
  )
import Hasura.GraphQL.Execute.RemoteJoin.Types
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReq (..), GQLReqOutgoing)
import Hasura.Prelude
import Hasura.RQL.IR.RemoteSchema (convertSelectionSet)
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ResultCustomization
import Hasura.RemoteSchema.SchemaCache
import Hasura.Session
import Hasura.Tracing (MonadTrace)
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G

-------------------------------------------------------------------------------
-- Executing a remote join

-- | Construct and execute a call to a remote schema for a remote join.
makeRemoteSchemaJoinCall ::
  (MonadError QErr m, MonadTrace m, MonadIO m) =>
  -- | Function to send a request over the network.
  (GQLReqOutgoing -> m BL.ByteString) ->
  -- | User information.
  UserInfo ->
  -- | Information about that remote join.
  RemoteSchemaJoin ->
  -- | Name of the field from the join arguments.
  FieldName ->
  -- | Mapping from 'JoinArgumentId' to its corresponding 'JoinArgument'.
  IntMap.IntMap JoinArgument ->
  -- | The resulting join index (see 'buildJoinIndex') if any.
  m (Maybe (IntMap.IntMap AO.Value))
makeRemoteSchemaJoinCall networkFunction userInfo remoteSchemaJoin jaFieldName joinArguments = do
  Tracing.newSpan ("Remote join to remote schema for field " <>> jaFieldName) do
    -- step 1: construct the internal intermediary representation
    maybeRemoteCall <-
      Tracing.newSpan "Resolve execution step for remote join field"
        $ buildRemoteSchemaCall remoteSchemaJoin joinArguments userInfo
    -- if there actually is a remote call:
    for maybeRemoteCall \remoteCall -> do
      -- step 2: execute it over the network
      responseValue <- executeRemoteSchemaCall networkFunction remoteCall
      -- step 3: build the join index
      Tracing.newSpan "Build remote join index"
        $ buildJoinIndex remoteCall responseValue

-------------------------------------------------------------------------------
-- Internal representation

-- | Intermediate type containing all of the information required to perform
-- a remote schema call, constructed from the static join information.
data RemoteSchemaCall = RemoteSchemaCall
  { rscCustomizer :: ResultCustomizer,
    rscGQLRequest :: GQLReqOutgoing,
    rscResponsePaths :: IntMap.IntMap ResponsePath
  }

-- | Used to extract the value from a remote schema response.
--
-- For example: if a remote relationship is defined to retrieve data from some
-- deeply nested field, this is the path towards that deeply nested field.
newtype ResponsePath = ResponsePath (NE.NonEmpty G.Name)
  deriving stock (Eq, Show)

-------------------------------------------------------------------------------
-- Step 1: building the remote call

-- | Constructs a 'RemoteSchemaCall' from some static information, such as the
-- definition of the join, and dynamic information such as the user's
-- information and the map of join arguments.
buildRemoteSchemaCall ::
  (MonadError QErr m) =>
  RemoteSchemaJoin ->
  IntMap.IntMap JoinArgument ->
  UserInfo ->
  m (Maybe RemoteSchemaCall)
buildRemoteSchemaCall RemoteSchemaJoin {..} arguments userInfo = do
  -- for each join argument, we generate a unique field, with the alias
  -- "f" <> argumentId
  fields <- flip IntMap.traverseWithKey arguments $ \argumentId (JoinArgument argument) -> do
    graphqlArgs <- fmap HashMap.fromList
      $ for (HashMap.toList argument) \(FieldName columnName, value) -> do
        graphQLName <- parseGraphQLName columnName
        graphQLValue <- ordJSONValueToGValue value
        pure (graphQLName, graphQLValue)
    -- Creating the alias should never fail.
    let aliasText = T.pack $ "f" <> show argumentId
    alias <-
      G.mkName aliasText
        `onNothing` throw500 ("'" <> aliasText <> "' is not a valid GraphQL name!")
    let responsePath = alias NE.:| fmap fcName (NE.tail _rsjFieldCall)
        rootField = fcName $ NE.head _rsjFieldCall
        resultCustomizer = applyAliasMapping (singletonAliasMapping rootField alias) _rsjResultCustomizer
    gqlField <- fieldCallsToField _rsjArgs graphqlArgs (convertSelectionSet _rsjSelSet) alias _rsjFieldCall
    pure (gqlField, responsePath, resultCustomizer)

  -- this constructs the actual GraphQL Request that can be sent to the remote
  for (NE.nonEmpty $ IntMap.elems fields) $ \neFields -> do
    gqlRequest <-
      fmap fieldsToRequest
        . runVariableCache
        . for neFields
        $ \(field, _, _) -> traverse (resolveRemoteVariable userInfo) field
    let customizer = foldMap (view _3) fields
        responsePath = fmap (ResponsePath . view _2) fields
    pure $ RemoteSchemaCall customizer gqlRequest responsePath

-- | Fold nested 'FieldCall's into a bare 'Field', inserting the passed
-- selection set at the leaf of the tree we construct.
fieldCallsToField ::
  forall m.
  (MonadError QErr m) =>
  -- | user input arguments to the remote join field
  HashMap.HashMap G.Name (P.InputValue RemoteSchemaVariable) ->
  -- | Contains the values of the variables that have been defined in the remote join definition
  HashMap.HashMap G.Name (G.Value Void) ->
  -- | Inserted at leaf of nested FieldCalls
  G.SelectionSet G.NoFragments RemoteSchemaVariable ->
  -- | Top-level name to set for this Field
  G.Name ->
  NonEmpty FieldCall ->
  m (G.Field G.NoFragments RemoteSchemaVariable)
fieldCallsToField rrArguments variables finalSelSet topAlias =
  fmap (\f -> f {G._fAlias = Just topAlias}) . nest
  where
    -- almost: `foldr nest finalSelSet`
    nest :: NonEmpty FieldCall -> m (G.Field G.NoFragments RemoteSchemaVariable)
    nest ((FieldCall name remoteArgs) :| rest) = do
      templatedArguments <- convert <$> createArguments variables remoteArgs
      graphQLarguments <- traverse peel rrArguments
      (args, selSet) <- case NE.nonEmpty rest of
        Just f -> do
          s <- nest f
          pure (templatedArguments, [G.SelectionField s])
        Nothing -> do
          arguments <-
            HashMap.unionWithM
              combineValues
              graphQLarguments
              -- converting (G.Value Void) -> (G.Value Variable) to merge the
              -- 'rrArguments' with the 'variables'
              templatedArguments
          pure (arguments, finalSelSet)
      pure $ G.Field Nothing name args [] selSet

    convert :: HashMap.HashMap G.Name (G.Value Void) -> HashMap.HashMap G.Name (G.Value RemoteSchemaVariable)
    convert = fmap G.literal

    peel :: P.InputValue RemoteSchemaVariable -> m (G.Value RemoteSchemaVariable)
    peel = \case
      P.GraphQLValue v -> pure v
      P.JSONValue _ ->
        -- At this point, it is theoretically impossible that we have
        -- unpacked a variable into a JSONValue, as there's no "outer
        -- scope" at which this value could have been peeled.
        -- FIXME: check that this is correct!
        throw500 "internal error: encountered an already expanded variable when folding remote field arguments"

-- | Create an argument map using the inputs taken from the left hand side.
createArguments ::
  (MonadError QErr m) =>
  HashMap.HashMap G.Name (G.Value Void) ->
  RemoteArguments ->
  m (HashMap G.Name (G.Value Void))
createArguments variables (RemoteArguments arguments) =
  toEither (traverse substituteVariables arguments)
    `onLeft` (\errors -> throw400 Unexpected $ "Found errors: " <> commaSeparated errors)
  where
    substituteVariables = \case
      G.VVariable variableName ->
        HashMap.lookup variableName variables
          `onNothing` Failure ["Value for variable " <> variableName <<> " not provided"]
      G.VList listValue ->
        fmap G.VList (traverse substituteVariables listValue)
      G.VObject objectValue ->
        fmap G.VObject (traverse substituteVariables objectValue)
      G.VInt i -> pure $ G.VInt i
      G.VFloat d -> pure $ G.VFloat d
      G.VString txt -> pure $ G.VString txt
      G.VEnum e -> pure $ G.VEnum e
      G.VBoolean b -> pure $ G.VBoolean b
      G.VNull -> pure $ G.VNull

-- | Combine two GraphQL values together.
--
-- This is used to combine different input arguments into one. This function can
-- only combine objects or lists pairwise, and fails if it has to combine any
-- other combination of values.
--
-- >>> combineValues (Object (fromList [("id", Number 1)]) (Object (fromList [("name", String "foo")])
-- Object (fromList [("id", Number 1), ("name", String "foo")])
combineValues ::
  (MonadError QErr m) =>
  G.Name ->
  G.Value RemoteSchemaVariable ->
  G.Value RemoteSchemaVariable ->
  m (G.Value RemoteSchemaVariable)
combineValues name v1 v2 = case (v1, v2) of
  (G.VObject l, G.VObject r) -> G.VObject <$> HashMap.unionWithM combineValues l r
  (G.VList l, G.VList r) -> pure $ G.VList $ l <> r
  (l, r) ->
    throw500
      $ "combineValues: cannot combine values ("
      <> tshow l
      <> ") and ("
      <> tshow r
      <> ") for field "
      <> G.unName name
      <> "; lists can only be merged with lists, objects can only be merged with objects"

-- | Craft a GraphQL query document from the list of fields.
fieldsToRequest :: NonEmpty (G.Field G.NoFragments P.Variable) -> GQLReqOutgoing
fieldsToRequest gFields =
  GQLReq
    { _grOperationName = Nothing,
      _grVariables =
        if HashMap.null variableInfos
          then Nothing
          else Just $ mapKeys G._vdName variableInfos,
      _grQuery =
        G.TypedOperationDefinition
          { G._todSelectionSet =
              -- convert from Field Variable to Field Name
              NE.toList $ G.SelectionField . fmap P.getName <$> gFields,
            G._todVariableDefinitions = HashMap.keys variableInfos,
            G._todType = G.OperationTypeQuery,
            G._todName = Nothing,
            G._todDirectives = []
          }
    }
  where
    variableInfos :: HashMap G.VariableDefinition J.Value
    variableInfos = HashMap.fromList $ concatMap (foldMap getVariableInfo) gFields
    getVariableInfo :: P.Variable -> [(G.VariableDefinition, J.Value)]
    getVariableInfo = pure . fmap snd . getVariableDefinitionAndValue

------------------------------------------------------------------------------
-- Step 2: sending the call over the network

-- | Sends the call over the network, and parse the resulting ByteString.
executeRemoteSchemaCall ::
  (MonadError QErr m) =>
  -- | Function to send a request over the network.
  (GQLReqOutgoing -> m BL.ByteString) ->
  -- | Information about that call.
  RemoteSchemaCall ->
  -- | Resulting JSON object
  m AO.Object
executeRemoteSchemaCall networkFunction (RemoteSchemaCall customizer request _) = do
  responseBody <- networkFunction request
  responseJSON <-
    AO.eitherDecode responseBody
      `onLeft` (\e -> throw500 $ "Remote server response is not valid JSON: " <> T.pack e)
  responseObject <- AO.asObject responseJSON `onLeft` throw500
  let errors = AO.lookup "errors" responseObject
  if
    | isNothing errors || errors == Just AO.Null ->
        case AO.lookup "data" responseObject of
          Nothing -> throw500 "\"data\" field not found in remote response"
          Just v ->
            let v' = applyResultCustomizer customizer v
             in AO.asObject v' `onLeft` throw500
    | otherwise ->
        throwError
          (err400 Unexpected "Errors from remote server")
            { qeInternal = Just $ ExtraInternal $ J.object ["errors" J..= (AO.fromOrdered <$> errors)]
            }

-------------------------------------------------------------------------------
-- Step 3: extracting the join index

-- | Construct a join index from the remote source's 'AO.Value' response.
--
-- This function extracts from the 'RemoteJoinCall' a mapping from
-- 'JoinArgumentId' to 'ResponsePath': from an integer that uniquely identifies
-- a join argument to the "path" at which we expect that value in the
-- response. With it, and with the actual reponse JSON value obtained from the
-- remote server, it constructs a corresponding mapping of, for each argument,
-- its extracted value.
--
-- If the response does not have value at any of the provided 'ResponsePath's,
-- throw a generic 'QErr'.
--
-- NOTE(jkachmar): If we switch to an 'Applicative' validator, we can collect
-- more than one missing 'ResponsePath's (rather than short-circuiting on the
-- first missing value).
buildJoinIndex ::
  forall m.
  (MonadError QErr m) =>
  RemoteSchemaCall ->
  AO.Object ->
  m (IntMap.IntMap AO.Value)
buildJoinIndex RemoteSchemaCall {..} response =
  for rscResponsePaths $ \(ResponsePath path) ->
    go (AO.Object response) (map G.unName . NE.toList $ path)
  where
    go :: AO.Value -> [Text] -> m AO.Value
    go value path = case path of
      [] -> pure value
      k : ks -> case value of
        AO.Object obj -> do
          objValue <-
            AO.lookup k obj
              `onNothing` throw500 ("failed to lookup key '" <> toTxt k <> "' in response")
          go objValue ks
        _ ->
          throw500
            $ "unexpected non-object json value found while path not empty: "
            <> commaSeparated path

-------------------------------------------------------------------------------
-- Local helpers

-- NOTE: Ideally this should be done at the remote relationship validation
-- layer.
--
-- When validating remote relationships, we should store the validated names so
-- that we don't need to continually re-validate them downstream.
parseGraphQLName :: (MonadError QErr m) => Text -> m G.Name
parseGraphQLName txt =
  G.mkName txt `onNothing` (throw400 RemoteSchemaError $ errMsg)
  where
    errMsg = txt <> " is not a valid GraphQL name"

ordJSONValueToGValue :: (MonadError QErr n) => AO.Value -> n (G.Value Void)
ordJSONValueToGValue =
  either (throw400 ValidationFailed . fromErrorMessage) pure . P.jsonToGraphQL . AO.fromOrdered
