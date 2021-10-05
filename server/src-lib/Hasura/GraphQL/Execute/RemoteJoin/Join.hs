module Hasura.GraphQL.Execute.RemoteJoin.Join
  ( processRemoteJoins
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                     as J
import qualified Data.Aeson.Ordered                             as JO
import qualified Data.Environment                               as Env
import qualified Data.HashMap.Strict                            as Map
import qualified Data.HashMap.Strict.Extended                   as Map
import qualified Data.HashMap.Strict.InsOrd                     as OMap
import qualified Data.HashSet                                   as HS
import qualified Data.IntMap.Strict                             as IntMap
import qualified Data.List.NonEmpty                             as NE
import qualified Data.Text                                      as T
import qualified Network.HTTP.Client                            as HTTP
import qualified Network.HTTP.Types                             as N

import           Control.Lens                                   ((^?))
import           Data.Aeson.Lens                                (_Integral, _Number)
import           Data.Tuple                                     (swap)

import qualified Hasura.GraphQL.Execute.Backend                 as EB
import           Hasura.GraphQL.Execute.Instances               ()
import qualified Hasura.GraphQL.Execute.RemoteJoin.RemoteSchema as RS
import qualified Hasura.GraphQL.Transport.Backend               as TB
import           Hasura.GraphQL.Transport.Instances             ()
import qualified Hasura.Logging                                 as L
import qualified Hasura.SQL.AnyBackend                          as AB
import qualified Hasura.Tracing                                 as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.GraphQL.Logging                         (MonadQueryLog)
import           Hasura.GraphQL.Transport.HTTP.Protocol         (GQLReqUnparsed)
import           Hasura.RQL.Types
import           Hasura.Server.Types                            (RequestId)
import           Hasura.Server.Version                          (HasVersion)
import           Hasura.Session
import qualified Language.GraphQL.Draft.Syntax                  as G

forRemoteJoins
  :: (Applicative f)
  => Maybe RemoteJoins
  -> a
  -> (RemoteJoins -> f a)
  -> f a
forRemoteJoins remoteJoins onNoJoins f =
  maybe (pure onNoJoins) f remoteJoins

processRemoteJoins
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , EB.MonadQueryTags m
     , MonadQueryLog m
     , Tracing.MonadTrace m
     )
  => RequestId
  -> L.Logger L.Hasura
  -> Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> EncJSON
  -> Maybe RemoteJoins
  -> GQLReqUnparsed
  -> m EncJSON
processRemoteJoins requestId logger env manager reqHdrs userInfo lhs joinTree gqlreq = do
  forRemoteJoins joinTree lhs $ \remoteJoins -> do
    lhsParsed <- onLeft (JO.eitherDecode $ encJToLBS lhs) (throw500 . T.pack)
    encJFromOrderedValue . runIdentity <$>
      processRemoteJoins_ requestId logger env manager
      reqHdrs userInfo (Identity lhsParsed) remoteJoins
      gqlreq

processRemoteJoins_
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , EB.MonadQueryTags m
     , MonadQueryLog m
     , Tracing.MonadTrace m
     , Traversable f
     )
  => RequestId
  -> L.Logger L.Hasura
  -> Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> f JO.Value
  -> RemoteJoins
  -> GQLReqUnparsed
  -> m (f JO.Value)
processRemoteJoins_ requestId logger env manager reqHdrs userInfo lhs joinTree gqlreq = do
  (compositeValue, joins) <- collectJoinArguments (assignJoinIds joinTree) lhs
  joinIndices <- fmap (IntMap.mapMaybe id) $ for joins $ \JoinArguments{..} -> do
    let joinArguments = IntMap.fromList $ map swap $ Map.toList _jalArguments
    case _jalJoin of
      RemoteJoinRemoteSchema remoteSchemaJoin -> do
        -- construct a remote call for
        remoteCall <- RS.buildRemoteSchemaCall userInfo remoteSchemaJoin joinArguments
        -- A remote call could be Nothing if there are no join arguments
        for remoteCall $ \rsc@(RS.RemoteSchemaCall _ _ _ responsePaths) -> do
          remoteResponse <-
            RS.getRemoteSchemaResponse env manager reqHdrs userInfo rsc
          -- extract the join values from the remote's response
          RS.buildJoinIndex remoteResponse responsePaths

      RemoteJoinSource sourceJoin childJoinTree -> AB.dispatchAnyBackend @TB.BackendTransport sourceJoin \(RemoteSourceJoin{..} :: RemoteSourceJoin b) -> do
        let rows = flip map (IntMap.toList joinArguments) $ \(argumentId, argument) ->
                Map.insert "__argument_id__" (J.toJSON argumentId) $
                Map.fromList $ map (getFieldNameTxt *** JO.fromOrdered) $
                Map.toList $ unJoinArgument argument
            rowSchema = fmap (\(_, rhsType, rhsColumn) -> (rhsColumn, rhsType)) _rsjJoinColumns

        for (NE.nonEmpty rows) $ \nonEmptyRows -> do
          stepInfo <- EB.mkDBRemoteRelationshipPlan
            userInfo _rsjSource _rsjSourceConfig nonEmptyRows rowSchema
            (FieldName "__argument_id__") (FieldName "f", _rsjRelationship)

          (_, sourceResponse) <- TB.runDBQuery @b
            requestId
            gqlreq
            -- NOTE: We're making an assumption that the 'FieldName' propagated
            -- upwards from 'collectJoinArguments' is reasonable to use for
            -- logging.
            (G.unsafeMkName . getFieldNameTxt $ _jalFieldName)
            userInfo
            logger
            _rsjSourceConfig
            (EB.dbsiAction stepInfo)
            (EB.dbsiPreparedQuery stepInfo)

          preRemoteJoinResults <- buildSourceDataJoinIndex sourceResponse
          forRemoteJoins childJoinTree preRemoteJoinResults $ \childRemoteJoins -> do
            results <- processRemoteJoins_ requestId logger env manager reqHdrs userInfo
              (IntMap.elems preRemoteJoinResults) childRemoteJoins gqlreq
            pure $ IntMap.fromAscList $ zip (IntMap.keys preRemoteJoinResults) results

  joinResults joinIndices compositeValue

-- | Attempt to construct a 'JoinIndex' from some 'EncJSON' source response.
buildSourceDataJoinIndex :: MonadError QErr m => EncJSON -> m JoinIndex
buildSourceDataJoinIndex response = do
  json <- JO.eitherDecode (encJToLBS response) `onLeft` \err ->
    throwInvalidJsonErr $ T.pack err
  case json of
    JO.Array arr -> fmap IntMap.fromList $ for (toList arr) \case
      JO.Object obj -> do
        argumentResult <-
          JO.lookup "f" obj
            `onNothing` throwMissingRelationshipDataErr
        argumentIdValue <-
          JO.lookup "__argument_id__" obj
            `onNothing` throwMissingArgumentIdErr
        argumentId <-
          (argumentIdValue ^? _Number . _Integral)
            `onNothing` throwInvalidArgumentIdTypeErr
        pure (argumentId, argumentResult)
      _ -> throwNoNestedObjectErr
    _ -> throwNoListOfObjectsErr
  where
    throwInvalidJsonErr errMsg =
      throw500 $
        "failed to decode JSON response from the source: " <> errMsg
    throwMissingRelationshipDataErr =
      throw500 $
        "cannot find relationship data (aliased as 'f') within the source \
        \response"
    throwMissingArgumentIdErr =
      throw500 $
        "cannot find '__argument_id__' within the source response"
    throwInvalidArgumentIdTypeErr =
      throw500 $ "expected 'argument_id' to be an integer"
    throwNoNestedObjectErr =
      throw500 $
        "expected an object one level deep in the remote schema's response, \
        \but found an array/scalar value instead"
    throwNoListOfObjectsErr =
      throw500 $
        "expected a list of objects in the remote schema's response, but found \
        \an object/scalar value instead"

type CompositeObject a = OMap.InsOrdHashMap Text (CompositeValue a)

-- | A hybrid JSON value representation which captures the context of remote join field in type parameter.
data CompositeValue a
  = CVOrdValue !JO.Value
  | CVObject !(CompositeObject a)
  | CVObjectArray ![CompositeValue a]
  | CVFromRemote !a
  deriving (Show, Eq, Functor, Foldable, Traversable)

compositeValueToJSON :: CompositeValue JO.Value -> JO.Value
compositeValueToJSON = \case
  CVOrdValue v       -> v
  CVObject obj       -> JO.object $ OMap.toList $ OMap.map compositeValueToJSON obj
  CVObjectArray vals -> JO.array $ map compositeValueToJSON vals
  CVFromRemote v     -> v

-- | A token used to uniquely identify the results within a join call that are
-- associated with a particular argument.
data ReplacementToken = ReplacementToken {
  _rtCallId     :: !JoinCallId,
  -- ^ Unique identifier for a remote join call.
  _rtArgumentId :: !JoinArgumentId
  -- ^ Unique identifier for an argument to some remote join.
}

joinResults
  :: forall f m
   . (MonadError QErr m, Traversable f)
  => IntMap.IntMap (IntMap.IntMap JO.Value)
  -> f (CompositeValue ReplacementToken)
  -> m (f JO.Value)
joinResults remoteResults compositeValues = do
  traverse (fmap compositeValueToJSON . traverse replaceToken) compositeValues
  where
    replaceToken :: ReplacementToken -> m JO.Value
    replaceToken (ReplacementToken joinCallId argumentId) = do
      joinCallResults <- onNothing (IntMap.lookup joinCallId remoteResults) $
        throw500 $ "couldn't find results for the join with id: "
        <> tshow joinCallId
      onNothing (IntMap.lookup argumentId joinCallResults) $
        throw500 $ "couldn't find a value for argument id in the join results: "
        <> tshow (argumentId, joinCallId)

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
assignJoinIds :: JoinTree RemoteJoin -> JoinTree (JoinCallId, RemoteJoin)
assignJoinIds joinTree =
  evalState (traverse assignId joinTree) (0, [])
  where
    assignId
      :: RemoteJoin
      -> State (JoinCallId, [(JoinCallId, RemoteJoin)]) (JoinCallId, RemoteJoin)
    assignId remoteJoin = do
      (joinCallId, joinIds) <- get
      let mJoinId = joinIds & find \(_, j) -> j == remoteJoin
      mJoinId `onNothing` do
        put (joinCallId + 1, (joinCallId, remoteJoin):joinIds)
        pure (joinCallId, remoteJoin)

collectJoinArguments
  :: forall f m
   . (MonadError QErr m, Traversable f)
  => JoinTree (JoinCallId, RemoteJoin)
  -> f JO.Value
  -> m (f (CompositeValue ReplacementToken), IntMap.IntMap JoinArguments)
collectJoinArguments joinTree lhs = do
  result <- flip runStateT (0, mempty) $ traverse (traverseValue joinTree) lhs
  -- Discard the 'JoinArgumentId' from the intermediate state transformation.
  pure $ second snd result
  where
    getReplacementToken
      :: IntMap.Key
      -> RemoteJoin
      -> JoinArgument
      -> FieldName
      -> StateT
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
          case Map.lookup argument arguments of
            Just argumentId -> pure $ ReplacementToken joinId argumentId
            Nothing         -> addNewArgument counter joins arguments
        Nothing -> addNewArgument counter joins mempty
      where
        addNewArgument counter joins arguments = do
          let argumentId = counter
              newArguments =
                JoinArguments
                  remoteJoin
                  (Map.insert argument argumentId arguments)
                  fieldName
          put (counter + 1, IntMap.insert joinId newArguments joins)
          pure $ ReplacementToken joinId argumentId

    traverseValue
      :: JoinTree (IntMap.Key, RemoteJoin)
      -> JO.Value
      -> StateT
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
      JO.Null          -> pure $ CVOrdValue JO.Null
      JO.Object object -> CVObject <$> traverseObject joinTree_ object
      JO.Array array   -> CVObjectArray <$> mapM (traverseValue joinTree_) (toList array)
      _                -> throw500 "found a scalar value when traversing with a non-empty join tree"

    traverseObject
      :: JoinTree (IntMap.Key, RemoteJoin)
      -> JO.Object
      -> StateT
           (JoinArgumentId, IntMap.IntMap JoinArguments)
           m
           (InsOrdHashMap Text (CompositeValue ReplacementToken))
    traverseObject joinTree_ object = do
      let phantomFields = HS.fromList $ map getFieldNameTxt $
                          concatMap (getPhantomFields . snd) $ toList joinTree_

          joinTreeNodes = Map.mapKeys getFieldNameTxt $ Map.fromList $
                          NE.toList $ unJoinTree joinTree_

      -- during this traversal we assume that the remote join column has some
      -- placeholder value in the response. If this weren't present it would
      -- involve a lot more book-keeping to preserve the order of the original
      -- selection set in the response
      compositeObject <- for (JO.toList object) $ \(fieldName, value_) ->
        (fieldName,) <$> case Map.lookup fieldName joinTreeNodes of
          Just (Leaf (joinId, remoteJoin)) -> do
            joinArgument <- forM (getJoinColumnMapping remoteJoin) $ \alias -> do
              let aliasTxt = getFieldNameTxt $ getAliasFieldName alias
              onNothing (JO.lookup aliasTxt object) $
                throw500 $ "a join column is missing from the response: " <> aliasTxt
            if Map.null (Map.filter (== JO.Null) joinArgument)
               then Just . CVFromRemote <$>
                 getReplacementToken joinId remoteJoin (JoinArgument joinArgument) (FieldName fieldName)
               -- we do not join with the remote field if any of the leaves of
               -- the join argument are null
               else pure $ Just $ CVOrdValue JO.Null
          Just (Tree joinSubTree) ->
            Just <$> traverseValue joinSubTree value_
          Nothing ->
            if HS.member fieldName phantomFields
               then pure Nothing
               else pure $ Just $ CVOrdValue value_

      pure . OMap.fromList $
        -- filter out the Nothings
        mapMaybe sequenceA compositeObject
