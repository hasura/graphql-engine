module Hasura.GraphQL.Execute.RemoteJoin.RemoteSchema
  ( buildRemoteSchemaCall,
    RemoteSchemaCall (..),
    getRemoteSchemaResponse,
    buildJoinIndex,
  )
where

import Control.Lens (view, _2, _3)
import Data.Aeson qualified as A
import Data.Aeson.Ordered qualified as AO
import Data.Environment qualified as Env
import Data.HashMap.Strict qualified as Map
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Extended (commaSeparated, toTxt, (<<>))
import Data.Validation (Validation (..), toEither)
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Remote
  ( collectVariablesFromSelectionSet,
    resolveRemoteVariable,
    runVariableCache,
  )
import Hasura.GraphQL.Execute.RemoteJoin.Types
import Hasura.GraphQL.Parser qualified as P
import Hasura.GraphQL.RemoteServer (execRemoteGQ)
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReq (..), GQLReqOutgoing)
import Hasura.Prelude
import Hasura.RQL.Types
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP

-- XXX(jkachmar): Think about reworking 'ResponsePath' to be 'Alias, Maybe [G.Name]'

-- | Used to extract the value from a remote schema response.
--
-- For example: if a remote relationship is defined to retrieve data from some
-- deeply nested field, this is the path towards that deeply nested field.
newtype ResponsePath = ResponsePath (NE.NonEmpty G.Name)
  -- (Alias, Maybe [G.Name])
  deriving stock (Eq, Show)

-- | The name that we generate when performing a remote join, which shall always
-- be the first field in a 'ResponsePath'.
type Alias = G.Name

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

-- | Intermediate type containing all of the information required to perform
-- a remote schema call.
--
-- See 'buildRemoteSchemaCall' for details.
data RemoteSchemaCall = RemoteSchemaCall
  { _rscInfo :: !RemoteSchemaInfo,
    _rscCustomizer :: !ResultCustomizer,
    _rscGQLRequest :: !GQLReqOutgoing,
    _rscResponsePaths :: !(IntMap.IntMap ResponsePath)
  }

-- | Constructs an outgoing response from the remote relationships definition
-- (i.e. 'RemoteSchemaJoin') and the arguments collected from the database's
-- response.
--
-- NOTE: We need to pass along some additional information with the raw outgoing
-- GraphQL request, hence the 'RemoteSchemaCall' type.
buildRemoteSchemaCall ::
  (MonadError QErr m) =>
  UserInfo ->
  RemoteSchemaJoin ->
  IntMap.IntMap JoinArgument ->
  m (Maybe RemoteSchemaCall)
buildRemoteSchemaCall userInfo RemoteSchemaJoin {..} arguments = do
  -- for each join argument, we generate a unique field, with the alias
  -- "f" <> argumentId
  fields <- flip IntMap.traverseWithKey arguments $ \argumentId (JoinArgument argument) -> do
    graphqlArgs <- fmap Map.fromList $
      for (Map.toList argument) $
        \(FieldName columnName, value) ->
          (,) <$> parseGraphQLName columnName <*> ordJSONValueToGValue value
    -- Creating the alias should never fail.
    let aliasText = T.pack $ "f" <> show argumentId
    alias <-
      G.mkName aliasText
        `onNothing` throw500 ("'" <> aliasText <> "' is not a valid GraphQL name!")
    let responsePath = alias NE.:| map fcName (toList $ NE.tail _rsjFieldCall)
        rootField = fcName $ NE.head _rsjFieldCall
        resultCustomizer = applyAliasMapping (singletonAliasMapping rootField alias) _rsjResultCustomizer
    gqlField <- fieldCallsToField _rsjArgs graphqlArgs _rsjSelSet alias _rsjFieldCall
    pure (gqlField, responsePath, resultCustomizer)

  -- this constructs the actual GraphQL Request that can be sent to the remote
  for (NE.nonEmpty $ IntMap.elems fields) $ \neFields -> do
    gqlRequest <-
      fmap fieldsToRequest . runVariableCache . for neFields $
        \(field, _, _) -> traverse (resolveRemoteVariable userInfo) field
    let customizer = foldMap (view _3) fields
        responsePath = fmap (ResponsePath . view _2) fields
    pure $ RemoteSchemaCall _rsjRemoteSchema customizer gqlRequest responsePath

-- | Construct a 'JoinIndex' from the remote source's 'AO.Value' response.
--
-- If the response does not have value at any of the provided 'ResponsePath's,
-- throw a generic 'QErr'.
--
-- NOTE(jkachmar): If we switch to an 'Applicative' validator, we can collect
-- more than one missing 'ResponsePath's (rather than short-circuiting on the
-- first missing value).
buildJoinIndex ::
  (Monad m, MonadError QErr m) =>
  AO.Object ->
  IntMap.IntMap ResponsePath ->
  m JoinIndex
buildJoinIndex response responsePaths =
  for responsePaths $ \path -> extractAtPath (AO.Object response) path

getRemoteSchemaResponse ::
  ( MonadError QErr m,
    MonadIO m,
    Tracing.MonadTrace m
  ) =>
  Env.Environment ->
  HTTP.Manager ->
  [HTTP.Header] ->
  UserInfo ->
  RemoteSchemaCall ->
  m AO.Object
getRemoteSchemaResponse env manager requestHeaders userInfo (RemoteSchemaCall rsi customizer req _) = do
  (_, _, respBody) <- execRemoteGQ env manager userInfo requestHeaders (rsDef rsi) req
  resp <-
    AO.eitherDecode respBody
      `onLeft` (\e -> throw500 $ "Remote server response is not valid JSON: " <> T.pack e)
  respObj <- AO.asObject resp `onLeft` throw500
  let errors = AO.lookup "errors" respObj
  if
      | isNothing errors || errors == Just AO.Null ->
        case AO.lookup "data" respObj of
          Nothing -> throw500 "\"data\" field not found in remote response"
          Just v ->
            let v' = applyResultCustomizer customizer v
             in AO.asObject v' `onLeft` throw500
      | otherwise ->
        throwError
          (err400 Unexpected "Errors from remote server")
            { qeInternal = Just $ ExtraInternal $ A.object ["errors" A..= (AO.fromOrdered <$> errors)]
            }

-- | Attempt to extract a deeply nested value from a remote source's 'AO.Value'
-- response, according to the JSON path provided by 'ResponsePath'.
extractAtPath ::
  forall m.
  MonadError QErr m =>
  AO.Value ->
  ResponsePath ->
  m AO.Value
extractAtPath initValue (ResponsePath rPath) =
  go initValue (map G.unName . NE.toList $ rPath)
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
          throw500 $
            "unexpected non-object json value found while path not empty: "
              <> commaSeparated path

ordJSONValueToGValue :: (MonadError QErr n) => AO.Value -> n (G.Value Void)
ordJSONValueToGValue =
  either (throw400 ValidationFailed) pure . P.jsonToGraphQL . AO.fromOrdered

convertFieldWithVariablesToName :: G.Field G.NoFragments P.Variable -> G.Field G.NoFragments G.Name
convertFieldWithVariablesToName = fmap P.getName

inputValueToJSON :: P.InputValue Void -> A.Value
inputValueToJSON = \case
  P.JSONValue j -> j
  P.GraphQLValue g -> graphQLValueToJSON g

-- | TODO: Documentation.
collectVariablesFromValue ::
  G.Value P.Variable -> HashMap G.VariableDefinition A.Value
collectVariablesFromValue = foldMap' \var@(P.Variable _ gType val) ->
  let name = P.getName var
      jsonVal = inputValueToJSON val
      defaultVal = getDefaultValue val
   in Map.singleton (G.VariableDefinition name gType defaultVal) jsonVal
  where
    getDefaultValue :: P.InputValue Void -> Maybe (G.Value Void)
    getDefaultValue = \case
      P.JSONValue _ -> Nothing
      P.GraphQLValue g -> Just g

-- | TODO: Documentation.
collectVariablesFromField ::
  G.Field G.NoFragments P.Variable -> HashMap G.VariableDefinition A.Value
collectVariablesFromField (G.Field _ _ arguments _ selSet) =
  let argumentVariables = fmap collectVariablesFromValue arguments
      selSetVariables =
        (fmap . fmap) snd $ collectVariablesFromSelectionSet selSet
   in fold' (Map.elems argumentVariables) <> Map.fromList selSetVariables

-- | TODO: Documentation.
--
-- Extension of the documentation required for 'collectVariablesFromField' and
-- 'collectVariablesFromValue'.
fieldsToRequest :: NonEmpty (G.Field G.NoFragments P.Variable) -> GQLReqOutgoing
fieldsToRequest gFields =
  let variableInfos = foldMap collectVariablesFromField gFields
   in GQLReq
        { _grOperationName = Nothing,
          _grVariables =
            mapKeys G._vdName variableInfos <$ guard (not $ Map.null variableInfos),
          _grQuery =
            G.TypedOperationDefinition
              { G._todSelectionSet =
                  NE.toList $ G.SelectionField . convertFieldWithVariablesToName <$> gFields,
                G._todVariableDefinitions = Map.keys variableInfos,
                G._todType = G.OperationTypeQuery,
                G._todName = Nothing,
                G._todDirectives = []
              }
        }

-- | Fold nested 'FieldCall's into a bare 'Field', inserting the passed
-- selection set at the leaf of the tree we construct.
fieldCallsToField ::
  forall m.
  MonadError QErr m =>
  -- | user input arguments to the remote join field
  Map.HashMap G.Name (P.InputValue RemoteSchemaVariable) ->
  -- | Contains the values of the variables that have been defined in the remote join definition
  Map.HashMap G.Name (G.Value Void) ->
  -- | Inserted at leaf of nested FieldCalls
  G.SelectionSet G.NoFragments RemoteSchemaVariable ->
  -- | Top-level name to set for this Field
  Alias ->
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
        Nothing ->
          let arguments =
                Map.unionWith
                  mergeValue
                  graphQLarguments
                  -- converting (G.Value Void) -> (G.Value Variable) to merge the
                  -- 'rrArguments' with the 'variables'
                  templatedArguments
           in pure (arguments, finalSelSet)
      pure $ G.Field Nothing name args [] selSet

    convert :: Map.HashMap G.Name (G.Value Void) -> Map.HashMap G.Name (G.Value RemoteSchemaVariable)
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

-- FIXME: better error message

-- This is a kind of "deep merge".
-- For e.g. suppose the input argument of the remote field is something like:
-- `where: { id : 1}`
-- And during execution, client also gives the input arg: `where: {name: "tiru"}`
-- We need to merge the input argument to where: {id : 1, name: "tiru"}
mergeValue :: G.Value RemoteSchemaVariable -> G.Value RemoteSchemaVariable -> G.Value RemoteSchemaVariable
mergeValue lVal rVal = case (lVal, rVal) of
  (G.VList l, G.VList r) ->
    G.VList $ l <> r
  (G.VObject l, G.VObject r) ->
    G.VObject $ Map.unionWith mergeValue l r
  (_, _) ->
    error $
      "can only merge a list with another list or an "
        <> "object with another object"

-- | Create an argument map using the inputs taken from the hasura database.
createArguments ::
  (MonadError QErr m) =>
  Map.HashMap G.Name (G.Value Void) ->
  RemoteArguments ->
  m (HashMap G.Name (G.Value Void))
createArguments variables (RemoteArguments arguments) =
  toEither (substituteVariables variables arguments)
    `onLeft` (\errors -> throw400 Unexpected $ "Found errors: " <> commaSeparated errors)

-- | Substitute values in the argument list.
substituteVariables ::
  -- | Values of the variables to substitute.
  HashMap G.Name (G.Value Void) ->
  -- | Template which contains the variables.
  HashMap G.Name (G.Value G.Name) ->
  Validation [Text] (HashMap G.Name (G.Value Void))
substituteVariables values = traverse go
  where
    go = \case
      G.VVariable variableName ->
        Map.lookup variableName values
          `onNothing` Failure ["Value for variable " <> variableName <<> " not provided"]
      G.VList listValue ->
        fmap G.VList (traverse go listValue)
      G.VObject objectValue ->
        fmap G.VObject (traverse go objectValue)
      G.VInt i -> pure $ G.VInt i
      G.VFloat d -> pure $ G.VFloat d
      G.VString txt -> pure $ G.VString txt
      G.VEnum e -> pure $ G.VEnum e
      G.VBoolean b -> pure $ G.VBoolean b
      G.VNull -> pure $ G.VNull
