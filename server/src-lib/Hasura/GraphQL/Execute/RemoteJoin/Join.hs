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
import qualified Data.List.NonEmpty                      as NE
import qualified Data.Text                               as T
import qualified Language.GraphQL.Draft.Syntax           as G
import qualified Network.HTTP.Client                     as HTTP
import qualified Network.HTTP.Types                      as N

import           Data.Text.Extended                      (commaSeparated, (<<>))
import           Data.Validation

import qualified Hasura.Tracing                          as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Remote
import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.GraphQL.Parser                   hiding (field)
import           Hasura.GraphQL.RemoteServer             (execRemoteGQ)
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types
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
processRemoteJoins env manager reqHdrs userInfo pgRes rjs = do
  -- Step 1: Decode the given bytestring as a JSON value
  jsonRes <- onLeft (AO.eitherDecode pgRes) (throw500 . T.pack)
  -- Step 2: Traverse through the JSON obtained in above step and generate composite JSON value with remote joins
  compositeJson <- traverseQueryResponseJSON rjMap jsonRes
  -- The remote server is queried only when all of the joining fields are *not* NULL:
  let remoteJoins = catMaybes $ collectRemoteFields compositeJson
  -- Step 3: Make queries to remote server and fetch graphql response
  remoteServerResp <- fetchRemoteJoinFields env manager reqHdrs userInfo remoteJoins
  -- Step 4: Replace remote fields in composite json with remote join values
  AO.toEncJSON <$> replaceRemoteFields compositeJson remoteServerResp
  where
    rjMap = Map.fromList $ toList rjs

type Alias = G.Name

-- | The counter which is used to append the alias generated for remote field. See 'pathToAlias'.
-- This guarentees the uniqueness of the alias.
newtype Counter = Counter {unCounter :: Int}
  deriving (Show, Eq)

incCounter :: Counter -> Counter
incCounter = Counter . (+1) . unCounter

getCounter :: MonadState Counter m => m Counter
getCounter = do
  c <- get
  modify incCounter
  pure c

parseGraphQLName :: (MonadError QErr m) => Text -> m G.Name
parseGraphQLName txt = onNothing (G.mkName txt) (throw400 RemoteSchemaError $ errMsg)
  where
    errMsg = txt <> " is not a valid GraphQL name"

-- | Generate the alias for remote field.
pathToAlias :: (MonadError QErr m) => FieldPath -> Counter -> m Alias
pathToAlias path counter =
  parseGraphQLName $ T.intercalate "_" (map getFieldNameTxt $ unFieldPath path)
                 <> "__" <> (tshow . unCounter) counter

type CompositeObject a = OMap.InsOrdHashMap Text (CompositeValue a)

-- | A hybrid JSON value representation which captures the context of remote join field in type parameter.
data CompositeValue a
  = CVOrdValue !AO.Value
  | CVObject !(CompositeObject a)
  | CVObjectArray ![CompositeValue a]
  | CVFromRemote !a
  deriving (Show, Eq, Functor, Foldable, Traversable)

collectRemoteFields :: CompositeValue a -> [a]
collectRemoteFields = toList

compositeValueToJSON :: CompositeValue AO.Value -> AO.Value
compositeValueToJSON = \case
  CVOrdValue v       -> v
  CVObject obj       -> AO.object $ OMap.toList $ OMap.map compositeValueToJSON obj
  CVObjectArray vals -> AO.array $ map compositeValueToJSON vals
  CVFromRemote v     -> v

-- | A 'RemoteJoinField' carries the minimal GraphQL AST of a remote relationship field.
-- All such 'RemoteJoinField's of a particular remote schema are batched together
-- and made GraphQL request to remote server to fetch remote join values.
data RemoteJoinField
  = RemoteJoinField
  { _rjfRemoteSchema :: !RemoteSchemaInfo -- ^ The remote schema server info.
  , _rjfAlias        :: !Alias -- ^ Top level alias of the field
  , _rjfField        :: !(G.Field G.NoFragments RemoteSchemaVariable) -- ^ The field AST
  , _rjfFieldCall    :: ![G.Name] -- ^ Path to remote join value
  } deriving (Show, Eq)

-- | Generate composite JSON ('CompositeValue') parameterised over 'RemoteJoinField'
--   from remote join map and query response JSON from Postgres.
traverseQueryResponseJSON
  :: (MonadError QErr m)
  => RemoteJoinMap
  -> AO.Value
  -> m (CompositeValue (Maybe RemoteJoinField))
traverseQueryResponseJSON rjm =
  flip runReaderT rjm . flip evalStateT (Counter 0) . traverseValue mempty
  where
    askRemoteJoins
      :: MonadReader RemoteJoinMap n
      => FieldPath
      -> n (Maybe (NE.NonEmpty RemoteJoin))
    askRemoteJoins path = asks (Map.lookup path)

    traverseValue
      :: (MonadError QErr n, MonadReader RemoteJoinMap n, MonadState Counter n)
      => FieldPath
      -> AO.Value
      -> n (CompositeValue (Maybe RemoteJoinField))
    traverseValue path = \case
      AO.Object obj -> traverseObject obj
      AO.Array arr  -> CVObjectArray <$> mapM (traverseValue path) (toList arr)
      v             -> pure $ CVOrdValue v
      where
        mkRemoteSchemaField
          :: (MonadError QErr n, MonadState Counter n)
          => AO.Object
          -> RemoteJoin
          -> n (Maybe RemoteJoinField)
        mkRemoteSchemaField siblingFields remoteJoin = runMaybeT $ do
          counter <- getCounter
          let RemoteJoin fieldName inputArgs selSet hasuraFields fieldCall rsi _ = remoteJoin
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
          for_ hasuraFields $ \(FieldName fieldNameTxt) -> do
            fldValue <- hoistMaybe $ AO.lookup fieldNameTxt siblingFields
            guard $ fldValue /= AO.Null
          hasuraFieldVariables <- lift $ mapM (parseGraphQLName . getFieldNameTxt) $ toList hasuraFields
          siblingFieldArgsVars <- lift $ mapM (\(k,val) -> do
                                           (,) <$> parseGraphQLName k <*> ordJSONValueToGValue val)
                                  $ AO.toList siblingFields
          let siblingFieldArgs = Map.fromList $ siblingFieldArgsVars
              hasuraFieldArgs = flip Map.filterWithKey siblingFieldArgs $ \k _ -> k `elem` hasuraFieldVariables
          fieldAlias <- lift $ pathToAlias (appendPath fieldName path) counter
          queryField <- lift $ fieldCallsToField (inputArgsToMap inputArgs) hasuraFieldArgs selSet fieldAlias fieldCall
          pure $ RemoteJoinField rsi
                                 fieldAlias
                                 queryField
                                 (map fcName $ toList $ NE.tail fieldCall)
          where
            ordJSONValueToGValue :: (MonadError QErr n) => AO.Value -> n (G.Value Void)
            ordJSONValueToGValue =
              either (throw400 ValidationFailed) pure . jsonToGraphQL . AO.fromOrdered

            inputArgsToMap = Map.fromList . map (_rfaArgument &&& _rfaValue)

        traverseObject obj = do
          let fields = AO.toList obj
          maybeRemoteJoins <- askRemoteJoins path
          processedFields <- fmap catMaybes $ forM fields $ \(fieldText, value) -> do
            let fieldName = FieldName fieldText
                fieldPath = appendPath fieldName path
            fmap (fieldText,) <$> case maybeRemoteJoins of
                Nothing -> Just <$> traverseValue fieldPath value
                Just nonEmptyRemoteJoins -> do
                  let remoteJoins = toList nonEmptyRemoteJoins
                      phantomColumnFields = concatMap _rjPhantomFields remoteJoins
                  if | fieldName `elem` phantomColumnFields -> pure Nothing
                     | otherwise -> do
                         case find ((== fieldName) . _rjName) remoteJoins of
                           Just rj -> Just . CVFromRemote <$> mkRemoteSchemaField obj rj
                           Nothing -> Just <$> traverseValue fieldPath value
          pure $ CVObject $ OMap.fromList processedFields

convertFieldWithVariablesToName :: G.Field G.NoFragments Variable -> G.Field G.NoFragments G.Name
convertFieldWithVariablesToName = fmap getName

inputValueToJSON :: InputValue Void -> A.Value
inputValueToJSON = \case
  JSONValue    j -> j
  GraphQLValue g -> graphQLValueToJSON g

defaultValue :: InputValue Void -> Maybe (G.Value Void)
defaultValue = \case
  JSONValue    _ -> Nothing
  GraphQLValue g -> Just g

collectVariablesFromValue :: G.Value Variable -> HashMap G.VariableDefinition A.Value
collectVariablesFromValue = \case
  G.VNull          -> mempty
  G.VInt _         -> mempty
  G.VFloat _       -> mempty
  G.VString _      -> mempty
  G.VBoolean _     -> mempty
  G.VEnum _        -> mempty
  G.VList values   -> foldl Map.union mempty $ map collectVariablesFromValue values
  G.VObject values -> foldl Map.union mempty $ map collectVariablesFromValue $ Map.elems values
  G.VVariable var@(Variable _ gType val) ->
    let name       = getName var
        jsonVal    = inputValueToJSON val
        defaultVal = defaultValue val
    in Map.singleton (G.VariableDefinition name gType defaultVal) jsonVal

collectVariablesFromField :: G.Field G.NoFragments Variable -> HashMap G.VariableDefinition A.Value
collectVariablesFromField (G.Field _ _ arguments _ selSet) =
  let argumentVariables = fmap collectVariablesFromValue arguments
      selSetVariables   = (fmap snd <$> collectVariablesFromSelectionSet selSet)
  in foldl Map.union mempty (Map.elems argumentVariables) <> Map.fromList selSetVariables

-- | Fetch remote join field value from remote servers by batching respective 'RemoteJoinField's
fetchRemoteJoinFields
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     )
  => Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> [RemoteJoinField]
  -> m AO.Object
fetchRemoteJoinFields env manager reqHdrs userInfo remoteJoins = do
  results <- forM (Map.toList remoteSchemaBatch) $ \(rsi, batch) -> do
    resolvedRemoteFields <- runVariableCache $ traverse (traverse (resolveRemoteVariable userInfo)) $ _rjfField <$> batch
    let gqlReq = fieldsToRequest resolvedRemoteFields
    -- NOTE: discard remote headers (for now):
    (_, _, respBody) <- execRemoteGQ env manager userInfo reqHdrs rsi gqlReq
    case AO.eitherDecode respBody of
      Left e -> throw500 $ "Remote server response is not valid JSON: " <> T.pack e
      Right r -> do
        respObj <- onLeft (AO.asObject r) throw500
        let errors = AO.lookup "errors" respObj
        if | isNothing errors || errors == Just AO.Null ->
               case AO.lookup "data" respObj of
                 Nothing -> throw400 Unexpected "\"data\" field not found in remote response"
                 Just v  -> onLeft (AO.asObject v) throw500

           | otherwise ->
             throwError (err400 Unexpected "Errors from remote server")
             {qeInternal = Just $ A.object ["errors" A..= (AO.fromOrdered <$> errors)]}
  onLeft (foldM AO.safeUnion AO.empty results) (throw500 . T.pack)
  where
    remoteSchemaBatch = Map.groupOnNE _rjfRemoteSchema remoteJoins

    fieldsToRequest :: NonEmpty (G.Field G.NoFragments Variable) -> GQLReqOutgoing
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

-- | Replace 'RemoteJoinField' in composite JSON with its json value from remote server response.
replaceRemoteFields
  :: MonadError QErr m
  => CompositeValue (Maybe RemoteJoinField)
  -> AO.Object
  -> m AO.Value
replaceRemoteFields compositeJson remoteServerResponse =
  compositeValueToJSON <$> traverse replaceValue compositeJson
  where
    -- `Nothing` below signifies that at-least one of the joining fields was NULL
    -- , when that happens we have to manually insert the `NULL` value for the
    -- remoteField value in the response.
    replaceValue Nothing = pure $ AO.Null
    replaceValue (Just RemoteJoinField {_rjfAlias, _rjfFieldCall}) =
      extractAtPath (_rjfAlias:_rjfFieldCall) $ AO.Object remoteServerResponse

    -- | 'FieldCall' is path to remote relationship value in remote server response.
    -- 'extractAtPath' traverse through the path and extracts the json value
    extractAtPath path v =
      case NE.nonEmpty path of
        Nothing          -> pure v
        Just (h :| rest) -> case v of
          AO.Object o   -> maybe
                           (throw500 $ "cannnot find value in remote response at path " <> tshow path)
                           (extractAtPath rest)
                           (AO.lookup (G.unName h) o)
          AO.Array arr -> AO.array <$> mapM (extractAtPath path) (toList arr)
          _            -> throw500 $ "expecting array or object in remote response at path " <> tshow path

-- | Fold nested 'FieldCall's into a bare 'Field', inserting the passed
-- selection set at the leaf of the tree we construct.
fieldCallsToField
  :: forall m. MonadError QErr m
  => Map.HashMap G.Name (InputValue RemoteSchemaVariable)
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

    peel :: InputValue RemoteSchemaVariable -> m (G.Value RemoteSchemaVariable)
    peel = \case
      GraphQLValue v -> pure v
      JSONValue _ ->
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
