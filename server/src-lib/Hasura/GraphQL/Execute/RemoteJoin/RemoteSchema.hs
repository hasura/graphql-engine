module Hasura.GraphQL.Execute.RemoteJoin.RemoteSchema
  ( buildRemoteSchemaCall
  , getRemoteSchemaResponse
  , buildJoinIndex
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                              as A
import qualified Data.Aeson.Ordered                      as AO
import qualified Data.Environment                        as Env
import qualified Data.HashMap.Strict                     as Map
import qualified Data.IntMap.Strict                      as IntMap
import qualified Data.List.NonEmpty                      as NE
import qualified Data.Text                               as T
import qualified Language.GraphQL.Draft.Syntax           as G
import qualified Network.HTTP.Client                     as HTTP
import qualified Network.HTTP.Types                      as N

import           Data.Text.Extended                      (commaSeparated, (<<>))
import           Data.Validation

import qualified Hasura.GraphQL.Parser                   as P
import qualified Hasura.Tracing                          as Tracing

import           Hasura.Base.Error
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types                        hiding (Alias)
import           Hasura.Server.Version                   (HasVersion)
import           Hasura.Session

type Alias = G.Name

parseGraphQLName :: (MonadError QErr m) => Text -> m G.Name
parseGraphQLName txt = onNothing (G.mkName txt) (throw400 RemoteSchemaError $ errMsg)
  where
    errMsg = txt <> " is not a valid GraphQL name"

buildRemoteSchemaCall
  :: (MonadError QErr m)
  => UserInfo
  -> RemoteSchemaJoin
  -> IntMap.IntMap JoinArgument
  -> m (Maybe (RemoteSchemaInfo, GQLReqOutgoing, IntMap.IntMap ResponsePath))
buildRemoteSchemaCall userInfo RemoteSchemaJoin{..} arguments = do
  fields <- flip IntMap.traverseWithKey arguments $ \argumentId (JoinArgument argument) -> do
    graphqlArgs <- fmap Map.fromList $ for (Map.toList argument) $
      \(FieldName columnName, value) ->
        (,) <$> parseGraphQLName columnName <*> ordJSONValueToGValue value
    let alias = G.unsafeMkName $ T.pack $ "f" <> show argumentId
        responsePath = alias NE.:| map fcName (toList $ NE.tail _rsjFieldCall)
    gqlField <- fieldCallsToField _rsjArgs graphqlArgs _rsjSelSet alias _rsjFieldCall
    pure (gqlField, responsePath)
  for (NE.nonEmpty $ IntMap.elems fields) $ \nonEmptyFields -> do
    gqlRequest <- fmap fieldsToRequest $ runVariableCache $
      traverse (traverse (resolveRemoteVariable userInfo) . fst) nonEmptyFields
    pure $ (_rsjRemoteSchema, gqlRequest, snd <$> fields)

buildJoinIndex
  :: (Monad m, MonadError QErr m)
  => AO.Object
  -> IntMap.IntMap ResponsePath
  -> m JoinIndex
buildJoinIndex response responsePaths =
  for responsePaths $ \path ->
  extractAtPath (AO.Object response) (map G.unName $ NE.toList path)

getRemoteSchemaResponse
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> RemoteSchemaInfo
  -> GQLReqOutgoing
  -> m AO.Object
getRemoteSchemaResponse env manager requestHeaders userInfo remoteSchema request = do
  (_, _, respBody) <- execRemoteGQ env manager userInfo requestHeaders remoteSchema request
  case AO.eitherDecode respBody of
    Left e -> throw500 $ "Remote server response is not valid JSON: " <> T.pack e
    Right r -> do
      respObj <- onLeft (AO.asObject r) throw500
      let errors = AO.lookup "errors" respObj
      if | isNothing errors || errors == Just AO.Null ->
             case AO.lookup "data" respObj of
               Nothing -> throw500 "\"data\" field not found in remote response"
               Just v  -> onLeft (AO.asObject v) throw500
         | otherwise ->
           throwError (err400 Unexpected "Errors from remote server")
           {qeInternal = Just $ A.object ["errors" A..= (AO.fromOrdered <$> errors)]}

extractAtPath
  :: MonadError QErr m
  => AO.Value -> [Text] -> m AO.Value
extractAtPath value path = case path of
  [] -> pure value
  k:rest -> case value of
    AO.Object o -> do
      v <- onNothing (AO.lookup k o) $
        throw500 $ "failed to lookup key in response"
      extractAtPath v rest
    _ ->
      throw500 $ "unexpected non-object json value found while path not empty: "
      <> commaSeparated path


ordJSONValueToGValue :: (MonadError QErr n) => AO.Value -> n (G.Value Void)
ordJSONValueToGValue =
  either (throw400 ValidationFailed) pure . P.jsonToGraphQL . AO.fromOrdered

convertFieldWithVariablesToName :: G.Field G.NoFragments P.Variable -> G.Field G.NoFragments G.Name
convertFieldWithVariablesToName = fmap P.getName

inputValueToJSON :: P.InputValue Void -> A.Value
inputValueToJSON = \case
  P.JSONValue    j -> j
  P.GraphQLValue g -> graphQLValueToJSON g

defaultValue :: P.InputValue Void -> Maybe (G.Value Void)
defaultValue = \case
  P.JSONValue    _ -> Nothing
  P.GraphQLValue g -> Just g

collectVariablesFromValue :: G.Value P.Variable -> HashMap G.VariableDefinition A.Value
collectVariablesFromValue = \case
  G.VNull          -> mempty
  G.VInt _         -> mempty
  G.VFloat _       -> mempty
  G.VString _      -> mempty
  G.VBoolean _     -> mempty
  G.VEnum _        -> mempty
  G.VList values   -> foldl Map.union mempty $ map collectVariablesFromValue values
  G.VObject values -> foldl Map.union mempty $ map collectVariablesFromValue $ Map.elems values
  G.VVariable var@(P.Variable _ gType val) ->
    let name       = P.getName var
        jsonVal    = inputValueToJSON val
        defaultVal = defaultValue val
    in Map.singleton (G.VariableDefinition name gType defaultVal) jsonVal

collectVariablesFromField :: G.Field G.NoFragments P.Variable -> HashMap G.VariableDefinition A.Value
collectVariablesFromField (G.Field _ _ arguments _ selSet) =
  let argumentVariables = fmap collectVariablesFromValue arguments
      selSetVariables   = (fmap snd <$> collectVariablesFromSelectionSet selSet)
  in foldl Map.union mempty (Map.elems argumentVariables) <> Map.fromList selSetVariables


fieldsToRequest :: NonEmpty (G.Field G.NoFragments P.Variable) -> GQLReqOutgoing
fieldsToRequest gFields =
  let variableInfos = foldMap collectVariablesFromField gFields
  in GQLReq
       { _grOperationName = Nothing
       , _grVariables =
           mapKeys G._vdName variableInfos <$ guard (not $ Map.null variableInfos)
       , _grQuery = G.TypedOperationDefinition
          { G._todSelectionSet =
              NE.toList $ G.SelectionField . convertFieldWithVariablesToName <$> gFields
          , G._todVariableDefinitions = Map.keys variableInfos
          , G._todType = G.OperationTypeQuery
          , G._todName = Nothing
          , G._todDirectives = []
          }
       }

-- | Fold nested 'FieldCall's into a bare 'Field', inserting the passed
-- selection set at the leaf of the tree we construct.
fieldCallsToField
  :: forall m. MonadError QErr m
  => Map.HashMap G.Name (P.InputValue RemoteSchemaVariable)
  -- ^ user input arguments to the remote join field
  -> Map.HashMap G.Name (G.Value Void)
  -- ^ Contains the values of the variables that have been defined in the remote join definition
  -> G.SelectionSet G.NoFragments RemoteSchemaVariable
  -- ^ Inserted at leaf of nested FieldCalls
  -> Alias
  -- ^ Top-level name to set for this Field
  -> NonEmpty FieldCall
  -> m (G.Field G.NoFragments RemoteSchemaVariable)
fieldCallsToField rrArguments variables finalSelSet topAlias =
  fmap (\f -> f{G._fAlias = Just topAlias}) . nest
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
              let arguments = Map.unionWith mergeValue
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
  (_, _) -> error $ "can only merge a list with another list or an " <>
                    "object with another object"

-- | Create an argument map using the inputs taken from the hasura database.
createArguments
  :: (MonadError QErr m)
  => Map.HashMap G.Name (G.Value Void)
  -> RemoteArguments
  -> m (HashMap G.Name (G.Value Void))
createArguments variables (RemoteArguments arguments) =
  onLeft
    (toEither (substituteVariables variables arguments))
    (throw400 Unexpected . \errors -> "Found errors: " <> commaSeparated errors)

-- | Substitute values in the argument list.
substituteVariables
  :: HashMap G.Name (G.Value Void) -- ^ Values of the variables to substitute.
  -> HashMap G.Name (G.Value G.Name) -- ^ Template which contains the variables.
  -> Validation [Text] (HashMap G.Name (G.Value Void))
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
