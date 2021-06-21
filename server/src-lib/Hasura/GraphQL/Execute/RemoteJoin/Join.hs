module Hasura.GraphQL.Execute.RemoteJoin.Join
  ( processRemoteJoins
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                                     as J
import qualified Data.Aeson.Ordered                             as AO
import qualified Data.Environment                               as Env
import qualified Data.HashMap.Strict                            as Map
import qualified Data.HashMap.Strict.Extended                   as Map
import qualified Data.HashMap.Strict.InsOrd                     as OMap
import qualified Data.HashSet                                   as HS
import qualified Data.IntMap.Strict                             as IntMap
import qualified Data.List.NonEmpty                             as NE
import qualified Data.Scientific                                as Scientific
import qualified Data.Text                                      as T
import qualified Network.HTTP.Client                            as HTTP
import qualified Network.HTTP.Types                             as N

import           Data.Tuple                                     (swap)

import qualified Hasura.GraphQL.Execute.Backend                 as EB
import qualified Hasura.GraphQL.Execute.RemoteJoin.RemoteSchema as RS
import qualified Hasura.Logging                                 as L
import qualified Hasura.SQL.AnyBackend                          as AB
import qualified Hasura.Tracing                                 as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Instances               ()
import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.GraphQL.Logging
import           Hasura.RQL.Types                               hiding (Alias)
import           Hasura.Server.Types                            (RequestId)
import           Hasura.Server.Version                          (HasVersion)
import           Hasura.Session

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
     , Tracing.MonadTrace m
     , MonadQueryLog m
     )
  => RequestId
  -> L.Logger L.Hasura
  -> Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> EncJSON
  -> Maybe RemoteJoins
  -> m EncJSON
processRemoteJoins requestId logger env manager reqHdrs userInfo lhs joinTree = do
  forRemoteJoins joinTree lhs $ \remoteJoins -> do
    lhsParsed <- onLeft (AO.eitherDecode $ encJToLBS lhs) (throw500 . T.pack)
    AO.toEncJSON . runIdentity <$>
      processRemoteJoins_ requestId logger env manager
      reqHdrs userInfo (Identity lhsParsed) remoteJoins

processRemoteJoins_
  :: ( HasVersion
     , MonadError QErr m
     , MonadIO m
     , Tracing.MonadTrace m
     , MonadQueryLog m
     , Traversable f
     )
  => RequestId
  -> L.Logger L.Hasura
  -> Env.Environment
  -> HTTP.Manager
  -> [N.Header]
  -> UserInfo
  -> f AO.Value
  -> RemoteJoins
  -> m (f AO.Value)
processRemoteJoins_ requestId logger env manager reqHdrs userInfo lhs joinTree = do


  (compositeValue, joins) <- collectJoinArguments (assignJoinIds joinTree) lhs

  joinIndices <- fmap (IntMap.mapMaybe id) $ for joins $ \JoinArguments{..} -> do

    let joinArguments = IntMap.fromList $ map swap $ Map.toList _jalArguments

    case _jalJoin of
      RemoteJoinRemoteSchema remoteSchemaJoin -> do
        -- construct a remote call for
        remoteCall <- RS.buildRemoteSchemaCall userInfo remoteSchemaJoin joinArguments

        -- A remote call could be Nothing if there are no join arguments
        for remoteCall $ \(remoteSchema, request, extractionPaths) -> do

          remoteResponse <- RS.getRemoteSchemaResponse env manager reqHdrs userInfo
            remoteSchema request

          -- extract the join values from the remote's response
          RS.buildJoinIndex remoteResponse extractionPaths

      RemoteJoinSource sourceJoin childJoinTree -> do
        AB.dispatchAnyBackend @EB.BackendExecute sourceJoin \RemoteSourceJoin{..} -> do

          let rows = flip map (IntMap.toList joinArguments) $ \(argumentId, argument) ->
                     Map.insert "__argument_id__" (J.toJSON argumentId) $
                     Map.fromList $ map (getFieldNameTxt *** AO.fromOrdered) $
                     Map.toList $ unJoinArugment argument
              rowSchema = fmap (\(_, rhsType, rhsColumn) -> (rhsColumn, rhsType)) _rdjJoinColumns

          for (NE.nonEmpty rows) $ \nonEmptyRows -> do
            sourceResponse <- EB.executeRemoteRelationship requestId logger
              userInfo _rdjSource _rdjSourceConfig nonEmptyRows rowSchema
              (FieldName "f", _rdjRelationship)
            preRemoteJoinResults <- buildSourceDataJoinIndex sourceResponse
            forRemoteJoins childJoinTree preRemoteJoinResults $ \childRemoteJoins -> do
              results <- processRemoteJoins_ requestId logger env manager reqHdrs userInfo
                (IntMap.elems preRemoteJoinResults) childRemoteJoins
              pure $ IntMap.fromAscList $ zip (IntMap.keys preRemoteJoinResults) results

  joinResults joinIndices compositeValue

  where
    buildSourceDataJoinIndex :: (MonadError QErr m) => EncJSON -> m JoinIndex
    buildSourceDataJoinIndex response = do
      jsonResponse <- onLeft (AO.eitherDecode $ encJToLBS response) $ \msg ->
        throw500 $ "error fetching a valid json response from source: " <> T.pack msg
      -- TODO: There should be a simpler way to write this
      case jsonResponse of
        AO.Array arr -> fmap IntMap.fromList $ forM (toList arr) $ \case
          AO.Object o ->  do
            argumentResult <- onNothing (AO.lookup "f" o) $ throw500 $
                "failed to lookup relationship data "
                <> "(aliased as 'f') from source's response"
            argumentIdValue <- onNothing (AO.lookup "__argument_id__" o) $ throw500 $
                "failed to lookup relationship data "
                <> "(aliased as 'f') from source's response"
            argumentId <- case argumentIdValue of
              AO.Number n ->
                onNothing (Scientific.toBoundedInteger n) $
                  throw500 "expecting an integer for argument_id"
              _           -> throw500 "expecting an integer for argument_id"
            pure (argumentId, argumentResult)
          _ -> throw500 $ "expecting an object at one level deep in "
               <> "remote schema's response, but found a scalar/array"
        _ -> throw500 $ "expecting a list of objects in "
             <> "remote schema's response, but found an object/scalar"

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

-- constructSelect
--   :: (EB.BackendExecute b)
--   => Map.HashMap FieldName (ScalarType b)
--   -> FieldName
--   -> RemoteSourceRelationship b
--   -> Map.HashMap JoinArgumentId JoinArgument
--   -> Maybe (IR.QueryDB b (P.UnpreparedValue b))
-- constructSelect fieldTypes fieldName relationship arguments =
--   IR.QDBMultipleRows . simpleSelect <$> selectFrom
--   where
--     selectFrom =
--       case rows of
--        [] -> Nothing
--        _  -> Just $ EB.buildTemporaryTable rows fieldTypes

--     rows = flip map (Map.toList arguments) $ \(argumentId, argument) ->
--              Map.fromList $ map (\(f, v) -> (getFieldNameTxt f, AO.fromOrdered v)) $
--                Map.toList $ unJoinArugment argument

--     field = case relationship of
--       RemoteSourceObject s         -> IR.AFObjectRelation s
--       RemoteSourceArray s          -> IR.AFArrayRelation $ IR.ASSimple s
--       RemoteSourceArrayAggregate s -> IR.AFArrayRelation $ IR.ASAggregate s

--     simpleSelect from =
--       IR.AnnSelectG { _asnFields = [(fieldName, field)]
--                     , _asnFrom = from
--                     , _asnPerm = IR.TablePerm annBoolExpTrue Nothing
--                     , _asnArgs = IR.noSelectArgs
--                     , _asnStrfyNum = False
--                     }

joinResults
  :: (MonadError QErr m, Traversable f)
  => IntMap.IntMap (IntMap.IntMap AO.Value)
  -> f (CompositeValue ReplacementToken)
  -> m (f AO.Value)
joinResults remoteResults compositeValues = do
  traverse (fmap compositeValueToJSON . traverse replaceToken) compositeValues
  where
    replaceToken (joinCallId, argumentId) = do
      joinCallResults <- onNothing (IntMap.lookup joinCallId remoteResults) $
        throw500 $ "couldn't find results for the join with id: "
        <> tshow joinCallId
      onNothing (IntMap.lookup argumentId joinCallResults) $
        throw500 $ "couldn't find a value for argument id in the join results: "
        <> tshow (argumentId, joinCallId)

-- When traversing a responses's json, wherever the join columns of a remote
-- join is expected, we want to collect these arguments. However looking up
-- by a remote join's definition to collect these arguments does not work
-- because we don't have an Ord or a Hashable instance (it would be a bit of
-- work). So this assigned each remote join a unique integer id by using just
-- the 'Eq' instance. The joinid then can be used for the collection of
-- arguments (which should also be faster)
assignJoinIds :: JoinTree RemoteJoin -> JoinTree (JoinCallId, RemoteJoin)
assignJoinIds joinTree =
  evalState (traverse assignId joinTree) (0, [])
  where
    assignId remoteJoin = do
      (counter, joinIds) <- get
      onNothing (find ((== remoteJoin) . snd) joinIds) $ do
        let joinId = counter
        put (counter + 1, (joinId, remoteJoin):joinIds)
        pure (joinId, remoteJoin)

collectJoinArguments
  :: (MonadError QErr m, Traversable f)
  => JoinTree (JoinCallId, RemoteJoin)
  -> f AO.Value
  -> m (f (CompositeValue ReplacementToken), IntMap.IntMap JoinArguments)
collectJoinArguments joinTree lhs =
  fmap (second snd) $ flip runStateT (0, mempty) $
   traverse (traverseValue joinTree) lhs
  where
    getReplacementToken joinId remoteJoin argument = do
      (counter, joins) <- get
      case IntMap.lookup joinId joins of
        Just (JoinArguments _remoteJoin arguments) ->
          case Map.lookup argument arguments of
            Just argumentId -> pure (joinId, argumentId)
            Nothing         -> addNewArgument counter joins arguments
        Nothing -> addNewArgument counter joins mempty
      where
        addNewArgument counter joins arguments = do
          let argumentId = counter
              newArguments = JoinArguments remoteJoin
                (Map.insert argument argumentId arguments)
          put (counter + 1, IntMap.insert joinId newArguments joins)
          pure (joinId, argumentId)

    traverseValue joinTree_ = \case
      AO.Object object -> CVObject <$> traverseObject joinTree_ object
      AO.Array array   -> CVObjectArray <$> mapM (traverseValue joinTree_) (toList array)
      _                -> throw500 "found a scalar value when traversing with a non-empty join tree"

    traverseObject joinTree_ object = do

      let phantomFields = HS.fromList $ map getFieldNameTxt $
                          concatMap (getPhantomFields . snd) $ toList joinTree_

          joinTreeNodes = Map.mapKeys getFieldNameTxt $ Map.fromList $
                          NE.toList $ unJoinTree joinTree_

      -- during this traversal we assume that the remote join column has some
      -- placeholder value in the response. If this weren't present it would
      -- involve a lot more book-keeping to preserve the order of the original
      -- selection set in the response
      compositeObject <- for (AO.toList object) $ \(fieldName, value_) ->
        (fieldName,) <$> case Map.lookup fieldName joinTreeNodes of
          Just (Leaf (joinId, remoteJoin)) -> do
            joinArgument <- forM (getJoinColumnMapping remoteJoin) $ \alias -> do
              let aliasTxt = getFieldNameTxt $ getAliasFieldName alias
              onNothing (AO.lookup aliasTxt object) $
                throw500 $ "a join column is missing from the response: " <> aliasTxt
            if Map.null (Map.filter (== AO.Null) joinArgument)
               then Just . CVFromRemote <$>
                 getReplacementToken joinId remoteJoin (JoinArgument joinArgument)
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


