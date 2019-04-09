module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import           Debug.Trace

import qualified Data.Aeson                             as J
import qualified Data.HashMap.Lazy                      as Map
import qualified Data.Vector                            as V
import qualified Database.PG.Query                      as Q
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as V
import qualified Language.GraphQL.Draft.Syntax          as G


runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> SchemaCache
  -> HTTP.Manager
  -> [N.Header]
  -> GraphQLRequest
  -> m EncJSON
runGQ pool isoL userInfo sqlGenCtx sc manager reqHdrs req = do
  execPlan <- E.getExecPlan userInfo sc req
  let (E.GQExecPlan hasuraPlan remotePlans) = execPlan
  case (hasuraPlan, remotePlans) of
     (Nothing, []) -> throw500 "no exec plan found"
     (Just (E.GExPHasura gCtx rootSelSet remRelPlans), []) -> runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet remRelPlans manager reqHdrs req
     _ -> runMixedGQ pool isoL userInfo sqlGenCtx manager reqHdrs req execPlan

runMixedGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool
  -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> HTTP.Manager
  -> [N.Header]
  -> GraphQLRequest
  -> E.GQExecPlan
  -> m EncJSON
runMixedGQ pool isoL userInfo sqlGenCtx manager reqHdrs req plan = do
  let E.GQExecPlan hasuraPlan remotePlans = plan
  allRes <- run (hasuraPlan, remotePlans)
  let interimResBS = map encJToLBS allRes
  interimRes <- forM interimResBS $ \res -> do
    let obj = J.decode res :: (Maybe J.Object)
    onNothing obj $ do
      liftIO $ print res
      throw500 "could not parse response as JSON"

  -- TODO: the order is not guaranteed! should we have a orderedmap?
  let datas = onlyObjs $ mapMaybe (Map.lookup "data") interimRes
      errs  = mapMaybe (Map.lookup "errors") interimRes

  return $ encJFromJValue $ J.object [ "data" J..= Map.unions datas
                                     , "errors" J..= errs
                                     ]
  where
    -- TODO: use async
    run (hasuraPlan, remotePlans) = do
      hasuraRes <- case hasuraPlan of
        Nothing -> return []
        Just (E.GExPHasura gCtx rootSelSet remRelPlans) -> do
          res <- runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet remRelPlans manager reqHdrs req
          return [res]
      remoteRes <- forM remotePlans $ \(E.GExPRemote rsi newq rs) ->
        E.execRemoteGQ manager userInfo reqHdrs newq rsi rs

      return $ hasuraRes ++ remoteRes

    -- TODO: should validate response and throw error?
    onlyObjs jVals =
      let fn jVal = case jVal of
            J.Object o -> Just o
            _          -> Nothing
      in mapMaybe fn jVals

runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool
  -> Q.TxIsolation
  -> UserInfo
  -> SQLGenCtx
  -> GCtx
  -> V.RootSelSet
  -> [E.GExPDepRemote]
  -> HTTP.Manager
  -> [N.Header]
  -> GraphQLRequest
  -> m EncJSON
runHasuraGQ pool isoL userInfo sqlGenCtx gCtx rootSelSet rrps manager reqHdrs req = do
  tx <- case rootSelSet of
    V.RQuery selSet ->
      return $ R.resolveQuerySelSet userInfo gCtx sqlGenCtx selSet
    V.RMutation selSet -> do
      assertNoRemRelPlans rrps
      return $ R.resolveMutSelSet userInfo gCtx sqlGenCtx selSet
    V.RSubscription _  ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  traceM (show $ encJToLBS resp)

  -- merge with remotes
  remotesRespTup <- forM rrps $ \rr -> do
    let E.GExPDepRemote rsi (E.GraphQLRequestPromise resolve) ji rs = rr
    -- TODO: use decodedResp instead of EncJSON
    traceShowM ("---------------------finding joinVals---------------")
    joinVals <- getJoinValues resp (E.jiParentAlias ji) (E.jiParentJoinKey ji)
    traceShowM joinVals
    let newq = resolve req joinVals
    remResp <- E.execRemoteGQ manager userInfo reqHdrs newq rsi rs
    traceShowM ("---------------------remote response---------------")
    traceShowM (encJToLBS remResp)
    return (ji, remResp)

  mergedResp <- mergeFields resp remotesRespTup

  return $ encodeGQResp $ GQSuccess $ encJToLBS mergedResp
  where
    runTx tx = runLazyTx pool isoL $ withUserInfo userInfo tx
    assertNoRemRelPlans [] = return ()
    assertNoRemRelPlans _ = throw400 UnexpectedPayload "remote fields are not supported in mutations"

    mergeFields :: (MonadError QErr m) => EncJSON -> [(E.JoinInfo, EncJSON)] -> m EncJSON
    mergeFields initResp remRespTups = do
      let init = encJToLBS initResp
          initObjM = J.decode init :: (Maybe J.Object)
      initObj <- onNothing initObjM $ throw500 "could not parse as json"
      mergedObj <- foldM mergeField initObj remRespTups
      return $ encJFromJValue mergedObj
      where
        mergeField initObj (ji, resp) = do
          let encResp = encJToLBS resp
              respObjM = J.decode encResp :: (Maybe J.Object)
              childKey = G.unName.G.unAlias $ E.jiChildAlias ji
              childPath = ["data", childKey]
              childJoinKey = G.unName $ E.jiChildJoinKey ji
              parentKey = G.unName.G.unAlias $ E.jiParentAlias ji
              parentPath = [parentKey]
              parentJoinKey = G.unName $ E.jiParentJoinKey ji

          respObj <- onNothing respObjM $ throw500 "could not parse as json"
          respVal <- getValueAtPath respObj childPath
          respArr <- assertArray respVal
          traceShowM "-------------------response array----------------"
          traceShowM respArr
          initVal <- getValueAtPath initObj parentPath
          initArr <- assertArray initVal
          traceShowM "-------------------init array----------------"
          traceShowM initArr

          newArr <- forM  (toList initArr) (\val -> findAndMerge val parentJoinKey (toList respArr) childJoinKey childKey)
          let newJArr = J.Array (V.fromList newArr)

          setValueAtPath initObj [] (parentKey, newJArr)


    findAndMerge :: (MonadError QErr m) => J.Value -> Text -> [J.Value] -> Text -> Text -> m J.Value
    findAndMerge parentVal parentKey values key mergeKey = do
      parentObj <- assertObject parentVal
      val <- getValueAtPath parentObj [parentKey]
      objects <- forM values assertObject
      let matchedObj = find (\obj -> Just val == Map.lookup key obj) objects
          insertObj = maybe "null" J.Object matchedObj

      newObj <- setValueAtPath parentObj [] (mergeKey, insertObj)
      return $ J.Object newObj

    getJoinValues :: (MonadError QErr m) => EncJSON -> G.Alias -> G.Name ->  m [J.Value]
    getJoinValues res name key = do
      let bs = encJToLBS res
          decodedResM = J.decode bs :: (Maybe J.Object)
      traceShowM bs
      decodedRes <- onNothing decodedResM $ throw500 "could not parse as JSON 1"
      let datas = Map.lookup (G.unName $ G.unAlias name) decodedRes
      joinVals <- case datas of
        Nothing -> throw500 "could not find parent field"
        Just vs -> do
          vals <- assertArray vs
          forM (toList vals) (`extractJoinKey` G.unName key)
      return $ catMaybes joinVals

    extractJoinKey val key = do
      obj <- assertObject val
      return $ Map.lookup key obj

getValueAtPath :: (MonadError QErr m) => J.Object -> [Text] -> m J.Value
getValueAtPath obj [] = return (J.Object obj)
getValueAtPath obj [x] = onNothing (Map.lookup x obj) $ throw500 "could not find any value at path"
getValueAtPath obj (x:xs) = do
  val <- getValueAtPath obj [x]
  valObj <- assertObject val
  getValueAtPath valObj xs

setValueAtPath :: (MonadError QErr m) => J.Object -> [Text] -> (Text, J.Value) -> m J.Object
setValueAtPath obj [] (k, newVal) = return $ Map.insert k newVal obj
setValueAtPath obj (x:xs) (k, newVal) = do
  val <- getValueAtPath obj [x]
  valObj <- assertObject val
  finalObj <- setValueAtPath valObj xs (k, newVal)
  setValueAtPath obj [] (x, J.Object finalObj)

assertObject :: (MonadError QErr m) => J.Value -> m J.Object
assertObject val = case val of
  J.Object obj -> return obj
  _            -> throw500 "could not parse as JSON object"

assertArray :: (MonadError QErr m) => J.Value -> m J.Array
assertArray val = case val of
  J.Array arr -> return arr
  _           -> throw500 "could not parse as JSON array"

