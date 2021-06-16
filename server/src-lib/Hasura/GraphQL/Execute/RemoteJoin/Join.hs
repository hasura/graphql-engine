module Hasura.GraphQL.Execute.RemoteJoin.Join
  ( processRemoteJoins
  , graphQLValueToJSON
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                              as A
import qualified Data.Aeson.Ordered                      as AO
import qualified Data.ByteString.Lazy                    as BL
import qualified Data.Environment                        as Env
import qualified Data.HashMap.Strict                     as Map
import qualified Data.HashMap.Strict.Extended            as Map
import qualified Data.HashMap.Strict.InsOrd              as OMap
import qualified Data.HashSet                            as HS
import qualified Data.List.NonEmpty                      as NE
import qualified Data.Text                               as T
import qualified Language.GraphQL.Draft.Syntax           as G
import qualified Network.HTTP.Client                     as HTTP
import qualified Network.HTTP.Types                      as N

import           Data.Text.Extended                      (commaSeparated, (<<>))
import           Data.Tuple                              (swap)
import           Data.Validation

import qualified Hasura.GraphQL.Parser                   as P
import qualified Hasura.Tracing                          as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types                        hiding (Alias)
import           Hasura.Server.Version                   (HasVersion)
import           Hasura.Session

processRemoteJoins
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> BL.ByteString
  -> RemoteJoins
  -> m EncJSON
processRemoteJoins env manager reqHdrs userInfo pgRes joinTree = do
  -- Step 1: Decode the given bytestring as a JSON value
  jsonRes <- onLeft (AO.eitherDecode pgRes) (throw500 . T.pack)

  (compositeValue, joinArguments) <- collectJoinArguments joinTree jsonRes

  joinIndices <- fmap (Map.fromList . catMaybes) $
    for (Map.toList joinArguments) $ \(remoteJoin, collectedArguments) -> do

    -- construct a remote call for
    remoteCall <- buildRemoteSchemaCall userInfo remoteJoin $ Map.fromList $
      map swap $ Map.toList $ _jalArguments collectedArguments

    -- A remote call could be Nothing if there are no join arguments
    for remoteCall $ \(remoteSchema, request, extractionPaths) -> do

      remoteResponse <- getRemoteSchemaResponse env manager reqHdrs userInfo
        remoteSchema request

      -- extract the join values from the remote's response
      joinIndex <- for extractionPaths $ \path -> do
        extractAtPath (AO.Object remoteResponse) (map G.unName $ NE.toList path)

      pure (_jalJoinCallId collectedArguments, joinIndex)

  AO.toEncJSON <$> joinResults joinIndices compositeValue

type Alias = G.Name

parseGraphQLName :: (MonadError QErr m) => Text -> m G.Name
parseGraphQLName txt = onNothing (G.mkName txt) (throw400 RemoteSchemaError $ errMsg)
  where
    errMsg = txt <> " is not a valid GraphQL name"

type CompositeObject a = OMap.InsOrdHashMap Text (CompositeValue a)

-- | A hybrid JSON value representation which captures the context of remote join field in type parameter.
data CompositeValue a
  = CVOrdValue !AO.Value
  | CVObject !(CompositeObject a)
  | CVObjectArray ![CompositeValue a]
  | CVFromRemote !a
  deriving (Show, Eq, Functor, Foldable, Traversable)

compositeValueToJSON :: CompositeValue AO.Value -> AO.Value
compositeValueToJSON = \case
  CVOrdValue v       -> v
  CVObject obj       -> AO.object $ OMap.toList $ OMap.map compositeValueToJSON obj
  CVObjectArray vals -> AO.array $ map compositeValueToJSON vals
  CVFromRemote v     -> v

newtype JoinCallId
  = JoinCallId { unJoinCallId :: Int }
  deriving (Show, Eq, Generic, Hashable, A.ToJSON)

newtype JoinArgumentId
  = JoinArgumentId { unArgumentId :: Int }
  deriving (Show, Eq, Generic, Hashable, A.ToJSON)

newtype JoinArgument
  = JoinArgument (Map.HashMap FieldName AO.Value)
  deriving (Show, Eq, Generic, Hashable)

type ReplacementToken = (JoinCallId, JoinArgumentId)

data JoinArguments
  = JoinArguments
  { _jalJoinCallId :: !JoinCallId
  , _jalArguments  :: !(Map.HashMap JoinArgument JoinArgumentId)
  } deriving (Show, Eq, Generic)

data ArgumentsCollectionState
  = ArgumentsCollectionState
  { _acsCounter :: !Int
  , _acsJoins   :: !(Map.HashMap RemoteJoin JoinArguments)
  } deriving (Show, Eq)

type ResponsePath = NE.NonEmpty G.Name

buildRemoteSchemaCall
  :: (MonadError QErr m)
  => UserInfo
  -> RemoteJoin
  -> Map.HashMap JoinArgumentId JoinArgument
  -> m (Maybe (RemoteSchemaInfo, GQLReqOutgoing, Map.HashMap JoinArgumentId ResponsePath))
buildRemoteSchemaCall userInfo RemoteJoin{..} arguments = do
  fields <- flip Map.traverseWithKey arguments $ \argumentId (JoinArgument argument) -> do
    graphqlArgs <- fmap Map.fromList $ for (Map.toList argument) $
      \(FieldName columnName, value) ->
        (,) <$> parseGraphQLName columnName <*> ordJSONValueToGValue value
    let alias = G.unsafeMkName $ T.pack $ "f" <> show (unArgumentId argumentId)
        responsePath = alias NE.:| map fcName (toList $ NE.tail _rjFieldCall)
    gqlField <- fieldCallsToField _rjArgs graphqlArgs _rjSelSet alias _rjFieldCall
    pure (gqlField, responsePath)
  for (NE.nonEmpty $ Map.elems fields) $ \nonEmptyFields -> do
    gqlRequest <- fmap fieldsToRequest $ runVariableCache $
      traverse (traverse (resolveRemoteVariable userInfo) . fst) nonEmptyFields
    pure $ (_rjRemoteSchema, gqlRequest, snd <$> fields)

joinResults
  :: (MonadError QErr m)
  => Map.HashMap JoinCallId (Map.HashMap JoinArgumentId AO.Value)
  -> CompositeValue ReplacementToken
  -> m AO.Value
joinResults remoteResults compositeValue =
  compositeValueToJSON <$> traverse replaceToken compositeValue
  where
    replaceToken (joinCallId, argumentId) = do
      joinCallResults <- onNothing (Map.lookup joinCallId remoteResults) $
        throw500 $ "couldn't find results for the join with id: "
        <> tshow (unJoinCallId joinCallId)
      onNothing (Map.lookup argumentId joinCallResults) $
        throw500 $ "couldn't find a value for argument id in the join results: "
        <> tshow (unArgumentId argumentId, unJoinCallId joinCallId)

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

collectJoinArguments
  :: (MonadError QErr m)
  => JoinTree RemoteJoin
  -> AO.Value
  -> m (CompositeValue ReplacementToken, Map.HashMap RemoteJoin JoinArguments)
collectJoinArguments joinTree value =
  fmap (second _acsJoins) $ flip runStateT (ArgumentsCollectionState 0 mempty) $
   traverseValue joinTree value
  where
    getReplacementToken remoteJoin argument = do
      (ArgumentsCollectionState counter joins) <- get
      case Map.lookup remoteJoin joins of
        Just (JoinArguments joinCallId arguments) ->
          case Map.lookup argument arguments of
            Just argumentId -> pure (joinCallId, argumentId)
            Nothing         -> addNewArgument counter joins joinCallId arguments
        Nothing -> addNewArgument counter joins (JoinCallId $ Map.size joins) mempty
      where
        addNewArgument counter joins joinCallId arguments = do
          let argumentId = JoinArgumentId counter
              newArguments = JoinArguments joinCallId
                (Map.insert argument argumentId arguments)
          put $ ArgumentsCollectionState (counter + 1) $
            Map.insert remoteJoin newArguments joins
          pure (joinCallId, argumentId)

    traverseValue joinTree_ = \case
      AO.Object object -> CVObject <$> traverseObject joinTree_ object
      AO.Array array   -> CVObjectArray <$> mapM (traverseValue joinTree_) (toList array)
      _                -> throw500 "found a scalar value when traversing with a non-empty join tree"

    traverseObject joinTree_ object = do

      let phantomFields = HS.fromList $
            mapMaybe (fmap getFieldNameTxt . getPhantomFieldName) $
            concatMap (Map.elems . _rjJoinColumnAliases . snd) $
            getLeaves joinTree_

          joinTreeNodes = Map.mapKeys getFieldNameTxt $ Map.fromList $
                          NE.toList joinTree_

      -- during this traversal we assume that the remote join column has some
      -- placeholder value in the response. If this weren't present it would
      -- involve a lot more book-keeping to preserve the order of the original
      -- selection set in the response
      compositeObject <- for (AO.toList object) $ \(fieldName, value_) ->
        (fieldName,) <$> case Map.lookup fieldName joinTreeNodes of
          Just (Leaf remoteJoin) -> do
            joinArgument <- forM (_rjJoinColumnAliases remoteJoin) $ \alias -> do
              let aliasTxt = getFieldNameTxt $ getAliasFieldName alias
              onNothing (AO.lookup aliasTxt object) $
                throw500 $ "a join column is missing from the response: " <> aliasTxt
            if Map.null (Map.filter (== AO.Null) joinArgument)
               then Just . CVFromRemote <$>
                 getReplacementToken remoteJoin (JoinArgument joinArgument)
               -- we do not join with the remote field if any of the leaves of
               -- the join argument are null
               else pure $ Just $ CVOrdValue AO.Null
          Just (Tree joinSubTree) ->
            Just <$> traverseValue joinSubTree value_
          Nothing ->
            if HS.member fieldName phantomFields
               then pure Nothing
               else pure $ Just $ CVOrdValue value_

      pure $ OMap.fromList $
        -- filter out the Nothings
        mapMaybe (\(k, v) -> (k,) <$> v) $ compositeObject

          -- when any of the joining fields are `NULL`, we don't query
          -- the remote schema
          --
          -- The rationale for performing such a join is that, postgres
          -- implements joins in the same way. For example:
          -- Let's say we have the following tables
          --
          -- test_db=# select * from users;
          -- id | first_name | last_name
          -- ----+------------+-----------
          --   4 | foo        |
          --   5 | baz        | bar
          --   6 | hello      |
          -- (3 rows)

          -- test_db=# select * from address;
          --  id | first_name | last_name | address
          -- ----+------------+-----------+----------
          --   4 | foo        |           | address1
          --   5 | baz        | bar       | address2
          --   6 |            | hello     | address3
          --
          -- Executing the following query:
          -- select u.first_name,u.last_name,a.address
          -- from users u
          -- left join address a on u.first_name = a.first_name
          -- and u.last_name = a.last_name;
          --
          -- gives the following result:
          --
          -- first_name | last_name | address
          -- -----------+-----------+----------
          --  baz       | bar       | address2
          --  foo       |           |
          --  hello     |           |
          --
          -- The data from the `address` table is fetched only when all
          -- of the arguments are **NOT** NULL.
          --
          -- For discussion of this design here
          -- see: https://github.com/hasura/graphql-engine-mono/pull/31#issuecomment-728230307


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
