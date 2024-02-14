module Hasura.GraphQL.Execute.RemoteJoin.Join
  ( processRemoteJoins,
    foldJoinTreeWith,
  )
where

import Control.Lens (view, _3)
import Control.Monad.Trans.Control
import Data.Aeson.Ordered qualified as JO
import Data.ByteString.Lazy qualified as BL
import Data.Environment qualified as Env
import Data.HashMap.Strict.Extended qualified as HashMap
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashMap.Strict.NonEmpty qualified as NEMap
import Data.HashSet qualified as HS
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T
import Data.Text.Extended (ToTxt (..))
import Data.Tuple (swap)
import Hasura.Backends.DataConnector.Agent.Client (AgentLicenseKey)
import Hasura.Base.Error
import Hasura.CredentialCache
import Hasura.EncJSON
import Hasura.GraphQL.Execute.Backend qualified as EB
import Hasura.GraphQL.Execute.Instances ()
import Hasura.GraphQL.Execute.RemoteJoin.RemoteSchema qualified as RS
import Hasura.GraphQL.Execute.RemoteJoin.Source qualified as S
import Hasura.GraphQL.Execute.RemoteJoin.Types
import Hasura.GraphQL.Logging (MonadExecutionLog, MonadQueryLog, statsToAnyBackend)
import Hasura.GraphQL.RemoteServer (execRemoteGQ)
import Hasura.GraphQL.Transport.Backend qualified as TB
import Hasura.GraphQL.Transport.HTTP.Protocol (GQLReqOutgoing, GQLReqUnparsed, _grOperationName, _unOperationName)
import Hasura.GraphQL.Transport.Instances ()
import Hasura.Logging qualified as L
import Hasura.Prelude
import Hasura.QueryTags
import Hasura.RQL.IR.ModelInformation (ModelInfoPart (..), ModelOperationType (ModelOperationType), ModelType (ModelTypeRemoteSchema))
import Hasura.RQL.Types.Common
import Hasura.RemoteSchema.SchemaCache
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Types (MonadGetPolicies, RequestId)
import Hasura.Services.Network
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Language.GraphQL.Draft.Syntax qualified as G
import Network.HTTP.Types qualified as HTTP

-------------------------------------------------------------------------------

-- | Process all remote joins, recursively.
--
-- Given the result of the first step of an execution and its associated remote
-- joins, process all joins recursively to build the resulting JSON object.
--
-- This function is a thin wrapper around 'processRemoteJoinsWith', and starts
-- the join tree traversal process by re-parsing the 'EncJSON' value into an
-- introspectable JSON 'Value', and "injects" the required functions to process
-- each join over the network.
processRemoteJoins ::
  forall m.
  ( MonadError QErr m,
    MonadIO m,
    MonadBaseControl IO m,
    MonadQueryTags m,
    MonadQueryLog m,
    MonadExecutionLog m,
    Tracing.MonadTrace m,
    ProvidesNetwork m,
    MonadGetPolicies m
  ) =>
  RequestId ->
  L.Logger L.Hasura ->
  Maybe (CredentialCache AgentLicenseKey) ->
  Env.Environment ->
  [HTTP.Header] ->
  UserInfo ->
  EncJSON ->
  Maybe RemoteJoins ->
  GQLReqUnparsed ->
  Tracing.HttpPropagator ->
  m (EncJSON, [ModelInfoPart])
processRemoteJoins requestId logger agentLicenseKey env requestHeaders userInfo lhs maybeJoinTree gqlreq tracesPropagator =
  Tracing.newSpan "Process remote joins" $ forRemoteJoins maybeJoinTree (lhs, []) \joinTree -> do
    lhsParsed <-
      JO.eitherDecode (encJToLBS lhs)
        `onLeft` (throw500 . T.pack)
    (jsonResult, modelInfoList) <-
      foldJoinTreeWith
        callSource
        callRemoteServer
        userInfo
        (Identity lhsParsed)
        joinTree
        requestHeaders
        (_unOperationName <$> _grOperationName gqlreq)
    pure $ (encJFromOrderedValue $ runIdentity jsonResult, (modelInfoList))
  where
    -- How to process a source join call over the network.
    callSource ::
      -- Generated information about the step
      AB.AnyBackend S.SourceJoinCall ->
      -- Resulting JSON object, as a 'ByteString'.
      m BL.ByteString
    callSource sourceJoinCall =
      AB.dispatchAnyBackend @TB.BackendTransport sourceJoinCall \(S.SourceJoinCall {..} :: S.SourceJoinCall b) -> do
        response <-
          TB.runDBQuery @b
            requestId
            gqlreq
            _sjcRootFieldAlias
            userInfo
            logger
            agentLicenseKey
            _sjcSourceConfig
            (fmap (statsToAnyBackend @b) (EB.dbsiAction _sjcStepInfo))
            (EB.dbsiPreparedQuery _sjcStepInfo)
            (EB.dbsiResolvedConnectionTemplate _sjcStepInfo)
        pure $ encJToLBS $ snd response

    -- How to process a remote schema join call over the network.
    callRemoteServer ::
      -- Information about the remote schema
      ValidatedRemoteSchemaDef ->
      -- Generated GraphQL request
      GQLReqOutgoing ->
      -- Resulting JSON object, as a 'ByteString'.
      m BL.ByteString
    callRemoteServer remoteSchemaInfo request =
      fmap (view _3)
        $ execRemoteGQ env tracesPropagator userInfo requestHeaders remoteSchemaInfo request

-- | Fold the join tree.
--
-- This function takes as an argument the functions that will be used to do the
-- actual network calls; this allows this function not to require 'MonadIO',
-- allowing it to be used in tests.
foldJoinTreeWith ::
  ( MonadError QErr m,
    MonadQueryTags m,
    Traversable f,
    Tracing.MonadTrace m,
    MonadIO m,
    MonadGetPolicies m
  ) =>
  -- | How to process a call to a source.
  (AB.AnyBackend S.SourceJoinCall -> m BL.ByteString) ->
  -- | How to process a call to a remote schema.
  (ValidatedRemoteSchemaDef -> GQLReqOutgoing -> m BL.ByteString) ->
  -- | User information.
  UserInfo ->
  -- | Initial accumulator; the LHS of this join tree.
  (f JO.Value) ->
  RemoteJoins ->
  [HTTP.Header] ->
  Maybe G.Name ->
  m (f JO.Value, [ModelInfoPart])
foldJoinTreeWith callSource callRemoteSchema userInfo lhs joinTree reqHeaders operationName = do
  (compositeValue, joins) <- collectJoinArguments (assignJoinIds joinTree) lhs
  (joinIndices) <- fmap catMaybes
    $ for joins
    $ \JoinArguments {..} -> do
      let joinArguments = IntMap.fromList $ map swap $ HashMap.toList _jalArguments
      (previousStep, modelInfo') <- case _jalJoin of
        RemoteJoinRemoteSchema remoteSchemaJoin childJoinTree -> do
          let remoteSchemaInfo = rsDef $ _rsjRemoteSchema remoteSchemaJoin
          maybeJoinIndex <- RS.makeRemoteSchemaJoinCall (callRemoteSchema remoteSchemaInfo) userInfo remoteSchemaJoin _jalFieldName joinArguments
          let remoteSchemaModel = ModelInfoPart (toTxt $ _vrsdName remoteSchemaInfo) ModelTypeRemoteSchema Nothing Nothing (ModelOperationType G.OperationTypeQuery)
          pure $ (fmap (childJoinTree,) maybeJoinIndex, Just [remoteSchemaModel])
        RemoteJoinSource sourceJoin childJoinTree -> do
          maybeJoinIndex <- S.makeSourceJoinCall callSource userInfo sourceJoin _jalFieldName joinArguments reqHeaders operationName
          pure $ (fmap (childJoinTree,) $ fst <$> maybeJoinIndex, snd <$> maybeJoinIndex)
      result <- for previousStep $ \((childJoinTree, joinIndex)) -> do
        forRemoteJoins childJoinTree (joinIndex, []) $ \childRemoteJoins -> do
          (results, modelInfo) <-
            foldJoinTreeWith
              callSource
              callRemoteSchema
              userInfo
              (IntMap.elems joinIndex)
              childRemoteJoins
              reqHeaders
              operationName
          pure $ ((IntMap.fromAscList $ zip (IntMap.keys joinIndex) results), modelInfo)
      pure $ fmap (\(iMap, newModelInfo) -> (iMap, newModelInfo <> fromMaybe [] modelInfo')) result
  let (key, (compositeValue')) = unzip (IntMap.toList joinIndices)
      (intMap, model) = unzip compositeValue'
      joinIndices' = IntMap.fromList $ zip key intMap
      modelInfoList = concat model
  Tracing.newSpan "Join remote join results"
    $ (,(modelInfoList))
    <$> (joinResults joinIndices' compositeValue)

-------------------------------------------------------------------------------

-- | Simple convenient wrapper around @Maybe RemoteJoins@.
forRemoteJoins ::
  (Applicative f) =>
  Maybe RemoteJoins ->
  a ->
  (RemoteJoins -> f a) ->
  f a
forRemoteJoins remoteJoins onNoJoins f =
  maybe (pure onNoJoins) f remoteJoins

-- | When traversing a responses's json, wherever the join columns of a remote
-- join are expected, we want to collect these arguments.
--
-- However looking up by a remote join's definition to collect these arguments
-- does not work because we don't have an 'Ord' or a 'Hashable' instance (it
-- would be a bit of work).
--
-- So this assigned each remote join a unique integer ID by using just the 'Eq'
-- instance. This ID then can be used for the collection of arguments (which
-- should also be faster).
--
-- TODO(nicuveo): https://github.com/hasura/graphql-engine-mono/issues/3891.
assignJoinIds :: JoinTree RemoteJoin -> JoinTree (JoinCallId, RemoteJoin)
assignJoinIds joinTree =
  evalState (traverse assignId joinTree) (0, [])
  where
    assignId ::
      RemoteJoin ->
      State (JoinCallId, [(JoinCallId, RemoteJoin)]) (JoinCallId, RemoteJoin)
    assignId remoteJoin = do
      (joinCallId, joinIds) <- get
      let mJoinId = joinIds & find \(_, j) -> j == remoteJoin
      mJoinId `onNothing` do
        put (joinCallId + 1, (joinCallId, remoteJoin) : joinIds)
        pure (joinCallId, remoteJoin)

collectJoinArguments ::
  forall f m.
  (MonadError QErr m, Traversable f) =>
  JoinTree (JoinCallId, RemoteJoin) ->
  f JO.Value ->
  m (f (CompositeValue ReplacementToken), IntMap.IntMap JoinArguments)
collectJoinArguments joinTree lhs = do
  result <- flip runStateT (0, mempty) $ traverse (traverseValue joinTree) lhs
  -- Discard the 'JoinArgumentId' from the intermediate state transformation.
  pure $ second snd result
  where
    getReplacementToken ::
      IntMap.Key ->
      RemoteJoin ->
      JoinArgument ->
      FieldName ->
      StateT
        (JoinArgumentId, IntMap.IntMap JoinArguments)
        m
        ReplacementToken
    getReplacementToken joinId remoteJoin argument fieldName = do
      (counter, joins) <- get
      case IntMap.lookup joinId joins of
        -- XXX: We're making an explicit decision to ignore the existing
        -- 'fieldName' and replace it with the argument provided to this
        -- function.
        --
        -- This needs to be tested so we can verify that the result of this
        -- function call is reasonable.
        Just (JoinArguments _remoteJoin arguments _fieldName) ->
          case HashMap.lookup argument arguments of
            Just argumentId -> pure $ ReplacementToken joinId argumentId
            Nothing -> addNewArgument counter joins arguments
        Nothing -> addNewArgument counter joins mempty
      where
        addNewArgument counter joins arguments = do
          let argumentId = counter
              newArguments =
                JoinArguments
                  remoteJoin
                  (HashMap.insert argument argumentId arguments)
                  fieldName
          put (counter + 1, IntMap.insert joinId newArguments joins)
          pure $ ReplacementToken joinId argumentId

    traverseValue ::
      JoinTree (IntMap.Key, RemoteJoin) ->
      JO.Value ->
      StateT
        (JoinArgumentId, IntMap.IntMap JoinArguments)
        m
        (CompositeValue ReplacementToken)
    traverseValue joinTree_ = \case
      -- 'JO.Null' is a special case of scalar value here, which indicates that
      -- the previous step did not return enough data for us to continue
      -- traversing down this path.
      --
      -- This can occur in the following cases:
      --  * Permission errors; when the user joins on a value they are not
      --    allowed to access
      --  * Queries with remote sources that resolve to null, for example:
      -- {
      --    q {
      --      user_by_pk() {
      --        id
      --        name
      --        r {
      --        }
      --        address {
      --          r_geo {
      --          }
      --        }
      --      }
      --    }
      -- }
      JO.Null -> pure $ CVOrdValue JO.Null
      JO.Object object -> CVObject <$> traverseObject joinTree_ object
      JO.Array array -> CVObjectArray <$> mapM (traverseValue joinTree_) (toList array)
      _ -> throw500 "found a scalar value when traversing with a non-empty join tree"

    traverseObject ::
      JoinTree (IntMap.Key, RemoteJoin) ->
      JO.Object ->
      StateT
        (JoinArgumentId, IntMap.IntMap JoinArguments)
        m
        (InsOrdHashMap Text (CompositeValue ReplacementToken))
    traverseObject joinTree_ object = do
      let joinTreeNodes = unJoinTree joinTree_
          phantomFields =
            HS.fromList
              $ map getFieldNameTxt
              $ concatMap (getPhantomFields . snd)
              $ toList joinTree_

      -- If we need the typename to disambiguate branches in the join tree, it
      -- will be present in the answer as a placeholder internal field.
      --
      -- We currently have no way of checking whether we explicitly requested
      -- that field, and it would be possible for a malicious user to attempt to
      -- spoof that value by explicitly requesting a value they control.
      -- However, there's no actual risk: we only use that value for lookups
      -- inside the join tree, and if we didn't request this field, the keys in
      -- the join tree map will explicitly require a typename NOT to be
      -- provided. Meaning that any spoofing attempt will just, at worst, result
      -- in remote joins not being performed.
      --
      -- We always remove that key from the resulting object.
      joinTypeName <- case JO.lookup "__hasura_internal_typename" object of
        Nothing -> pure Nothing
        Just (JO.String typename) -> pure $ Just typename
        Just value -> throw500 $ "The reserved __hasura_internal_typename field contains an unexpected value: " <> tshow value

      -- during this traversal we assume that the remote join column has some
      -- placeholder value in the response. If this weren't present it would
      -- involve a lot more book-keeping to preserve the order of the original
      -- selection set in the response
      compositeObject <- for (JO.toList object) $ \(fieldName, value_) ->
        (fieldName,) <$> case NEMap.lookup (QualifiedFieldName joinTypeName fieldName) joinTreeNodes of
          Just (Leaf (joinId, remoteJoin)) -> do
            joinArgument <- forM (getJoinColumnMapping remoteJoin) $ \alias -> do
              let aliasTxt = getFieldNameTxt $ getAliasFieldName alias
              onNothing (JO.lookup aliasTxt object)
                $ throw500
                $ "a join column is missing from the response: "
                <> aliasTxt
            if HashMap.null (HashMap.filter (== JO.Null) joinArgument)
              then
                Just
                  . CVFromRemote
                  <$> getReplacementToken joinId remoteJoin (JoinArgument joinArgument) (FieldName fieldName)
              else -- we do not join with the remote field if any of the leaves of
              -- the join argument are null
                pure $ Just $ CVOrdValue JO.Null
          Just (Tree joinSubTree) ->
            Just <$> traverseValue joinSubTree value_
          Nothing ->
            if HS.member fieldName phantomFields || fieldName == "__hasura_internal_typename"
              then pure Nothing
              else pure $ Just $ CVOrdValue value_

      pure
        . InsOrdHashMap.fromList
        $
        -- filter out the Nothings
        mapMaybe sequenceA compositeObject

joinResults ::
  forall f m.
  (MonadError QErr m, Traversable f) =>
  IntMap.IntMap (IntMap.IntMap JO.Value) ->
  f (CompositeValue ReplacementToken) ->
  m (f JO.Value)
joinResults remoteResults compositeValues = do
  traverse (fmap compositeValueToJSON . traverse replaceToken) compositeValues
  where
    replaceToken :: ReplacementToken -> m JO.Value
    replaceToken (ReplacementToken joinCallId argumentId) = do
      joinCallResults <-
        onNothing (IntMap.lookup joinCallId remoteResults)
          $ throw500
          $ "couldn't find results for the join with id: "
          <> tshow joinCallId
      onNothing (IntMap.lookup argumentId joinCallResults)
        $ throw500
        $ "couldn't find a value for argument id in the join results: "
        <> tshow (argumentId, joinCallId)

-------------------------------------------------------------------------------

type CompositeObject a = InsOrdHashMap.InsOrdHashMap Text (CompositeValue a)

-- | A hybrid JSON value representation which captures the context of remote join field in type parameter.
data CompositeValue a
  = CVOrdValue !JO.Value
  | CVObject !(CompositeObject a)
  | CVObjectArray ![CompositeValue a]
  | CVFromRemote !a
  deriving (Show, Eq, Functor, Foldable, Traversable)

compositeValueToJSON :: CompositeValue JO.Value -> JO.Value
compositeValueToJSON = \case
  CVOrdValue v -> v
  CVObject obj -> JO.object $ InsOrdHashMap.toList $ InsOrdHashMap.map compositeValueToJSON obj
  CVObjectArray vals -> JO.array $ map compositeValueToJSON vals
  CVFromRemote v -> v

-- | A token used to uniquely identify the results within a join call that are
-- associated with a particular argument.
data ReplacementToken = ReplacementToken
  { -- | Unique identifier for a remote join call.
    _rtCallId :: !JoinCallId,
    -- | Unique identifier for an argument to some remote join.
    _rtArgumentId :: !JoinArgumentId
  }
