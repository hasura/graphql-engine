module Hasura.GraphQL.Execute.RemoteJoin.Join
  ( processRemoteJoins
  ) where

import           Hasura.Prelude

import qualified Data.Aeson.Ordered                             as AO
import qualified Data.ByteString.Lazy                           as BL
import qualified Data.Environment                               as Env
import qualified Data.HashMap.Strict                            as Map
import qualified Data.HashMap.Strict.Extended                   as Map
import qualified Data.HashMap.Strict.InsOrd                     as OMap
import qualified Data.HashSet                                   as HS
import qualified Data.List.NonEmpty                             as NE
import qualified Data.Text                                      as T
import qualified Network.HTTP.Client                            as HTTP
import qualified Network.HTTP.Types                             as N

import           Data.Tuple                                     (swap)

import qualified Hasura.GraphQL.Execute.Backend                 as EB
import qualified Hasura.GraphQL.Execute.RemoteJoin.RemoteSchema as RS
import qualified Hasura.GraphQL.Parser                          as P
import qualified Hasura.RQL.IR                                  as IR
import qualified Hasura.Tracing                                 as Tracing

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.RemoteJoin.Types
import           Hasura.RQL.Types                               hiding (Alias)
import           Hasura.Server.Version                          (HasVersion)
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
    remoteCall <- RS.buildRemoteSchemaCall userInfo remoteJoin $ Map.fromList $
      map swap $ Map.toList $ _jalArguments collectedArguments

    -- A remote call could be Nothing if there are no join arguments
    for remoteCall $ \(remoteSchema, request, extractionPaths) -> do

      remoteResponse <- RS.getRemoteSchemaResponse env manager reqHdrs userInfo
        remoteSchema request

      -- extract the join values from the remote's response
      joinIndex <- RS.buildJoinIndex remoteResponse extractionPaths

      pure (_jalJoinCallId collectedArguments, joinIndex)

  AO.toEncJSON <$> joinResults joinIndices compositeValue

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

data ArgumentsCollectionState
  = ArgumentsCollectionState
  { _acsCounter :: !Int
  , _acsJoins   :: !(Map.HashMap RemoteJoin JoinArguments)
  } deriving (Show, Eq)

constructSelect
  :: (EB.BackendExecute b)
  => Map.HashMap JoinArgumentId JoinArgument
  -> Map.HashMap FieldName (ScalarType b)
  -> FieldName
  -> RemoteSourceRelationship b
  -> IR.QueryDB b (P.UnpreparedValue b)
constructSelect arguments fieldTypes fieldName relationship =
  IR.QDBMultipleRows simpleSelect
  where
    rows = flip map (Map.toList arguments) $ \(argumentId, argument) ->
             Map.fromList $ map (\(f, v) -> (getFieldNameTxt f, AO.fromOrdered v)) $
               Map.toList $ unJoinArugment argument

    field = case relationship of
      RemoteSourceObject s         -> IR.AFObjectRelation s
      RemoteSourceArray s          -> IR.AFArrayRelation $ IR.ASSimple s
      RemoteSourceArrayAggregate s -> IR.AFArrayRelation $ IR.ASAggregate s

    simpleSelect =
      IR.AnnSelectG { _asnFields = [(fieldName, field)]
                    , _asnFrom = EB.buildTemporaryTable rows fieldTypes
                    , _asnPerm = IR.TablePerm annBoolExpTrue Nothing
                    , _asnArgs = IR.noSelectArgs
                    , _asnStrfyNum = False
                    }

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
            map getFieldNameTxt$
            concatMap (getPhantomFields . snd) $
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
            joinArgument <- forM (_rsjJoinColumnAliases remoteJoin) $ \alias -> do
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


