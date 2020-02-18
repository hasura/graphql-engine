{-# LANGUAGE RecordWildCards #-}
module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , runGQBatched
  ) where

import qualified Data.Aeson                             as J
import qualified Data.Aeson.Ordered                     as JO
import qualified Data.Text                              as T
import qualified Data.Vector                            as V
import qualified Network.HTTP.Types                     as N
import qualified VectorBuilder.Vector                   as VB

import           Hasura.EncJSON
import           Hasura.GraphQL.Logging
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context
import           Hasura.Server.Utils                    (RequestId)
import           Hasura.Server.Version                  (HasVersion)

import qualified Database.PG.Query                      as Q
import qualified Hasura.GraphQL.Execute                 as E
import qualified Hasura.GraphQL.Execute.RemoteJoins     as E
import qualified Hasura.Logging                         as L
import qualified Hasura.Server.Telemetry.Counters       as Telem
import qualified Language.GraphQL.Draft.Printer.Text    as G
import qualified Language.GraphQL.Draft.Syntax          as G

runGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> GQLReq GQLQueryText
  -> m (HttpResponse EncJSON)
runGQ reqId userInfo reqHdrs req = do
  -- The response and misc telemetry data:
  let telemTransport = Telem.HTTP
  (telemTimeTot_DT, (telemCacheHit, telemLocality, (telemTimeIO_DT, telemQueryType, !resp))) <- withElapsedTime $ do
    E.ExecutionCtx _ sqlGenCtx pgExecCtx planCache sc scVer httpManager enableAL <- ask
    (telemCacheHit, execPlan) <- E.getResolvedExecPlan pgExecCtx planCache
                                 userInfo sqlGenCtx enableAL sc scVer httpManager reqHdrs req
    case execPlan of
      E.GExPHasura execOp -> do
        (telemTimeIO, telemQueryType, resp) <- runHasuraGQ reqId reqHdrs req userInfo execOp
        pure $ (telemCacheHit, Telem.Local, (telemTimeIO, telemQueryType, HttpResponse resp Nothing))
      E.GExPRemote rsi opDef  -> do
        let telemQueryType | G._todType opDef == G.OperationTypeMutation = Telem.Mutation
                            | otherwise = Telem.Query
        (telemTimeIO, resp) <- E.execRemoteGQ reqId userInfo reqHdrs req rsi $ G._todType opDef
        pure (telemCacheHit, Telem.Remote, (telemTimeIO, telemQueryType, resp))

  let telemTimeIO = fromUnits telemTimeIO_DT
      telemTimeTot = fromUnits telemTimeTot_DT
  Telem.recordTimingMetric Telem.RequestDimensions{..} Telem.RequestTimings{..}
  return resp

runGQBatched
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> UserInfo
  -> [N.Header]
  -> GQLBatchedReqs GQLQueryText
  -> m (HttpResponse EncJSON)
runGQBatched reqId userInfo reqHdrs reqs =
  case reqs of
    GQLSingleRequest req ->
      runGQ reqId userInfo reqHdrs req
    GQLBatchedReqs batch -> do
      -- It's unclear what we should do if we receive multiple
      -- responses with distinct headers, so just do the simplest thing
      -- in this case, and don't forward any.
      let removeHeaders =
            flip HttpResponse Nothing
            . encJFromList
            . map (either (encJFromJValue . encodeGQErr False) _hrBody)
          try = flip catchError (pure . Left) . fmap Right
      fmap removeHeaders $
        traverse (try . runGQ reqId userInfo reqHdrs) batch

runHasuraGQ
  :: ( HasVersion
     , MonadIO m
     , MonadError QErr m
     , MonadReader E.ExecutionCtx m
     )
  => RequestId
  -> [N.Header]
  -> GQLReqUnparsed
  -> UserInfo
  -> E.ExecOp
  -> m (DiffTime, Telem.QueryType, EncJSON)
  -- ^ Also return 'Mutation' when the operation was a mutation, and the time
  -- spent in the PG query; for telemetry.
runHasuraGQ reqId reqHdrs query userInfo resolvedOp = do
  exeCtx@(E.ExecutionCtx logger _ pgExecCtx _ _ _ _ _) <- ask
  (telemTimeIO, respE) <- withElapsedTime $ liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery queryOp genSql -> do
      -- log the generated SQL and the graphql query
      L.unLogger logger $ QueryLog query genSql reqId
      case queryOp of
        E.EQOSimple tx -> runLazyTx' pgExecCtx tx
        E.EQOComposite fieldsSet -> do
          resps <- fmap (toList) $ forM fieldsSet $ \case
            E.EQFPlain tx -> do
              r <- runLazyTx' pgExecCtx tx
              either (throw400 Unexpected . T.pack) pure $ parseEncJObject r
            E.EQFRemoteJoin remoteRelPlans tx -> do
              initJson <- runLazyTx' pgExecCtx tx
              let (initValue, joinInputs) = E.extractRemoteRelArguments
                                            (encodeGQResp $ GQSuccess $ encJToLBS initJson)
                                            (map E.rrpRemoteRelField $ toList remoteRelPlans)

              let batchesRemotePlans = map (uncurry E.mkQuery) $ catMaybes $ map sequence $
                                       zip (toList remoteRelPlans) joinInputs

              results <- forM batchesRemotePlans $
                \(E.GQRemoteRelPlan remoteRelSplice rsi, batch) -> do
                  gqlReq <- E.fieldsToRequest G.OperationTypeQuery batch
                  let gqlReqUnparsed = (GQLQueryText . G.renderExecutableDoc . G.ExecutableDocument . unGQLExecDoc) <$> gqlReq
                  (_, respBody) <- flip runReaderT exeCtx $
                                   E.execRemoteGQ reqId userInfo reqHdrs gqlReqUnparsed rsi G.OperationTypeQuery
                  -- NOTE: discard remote headers (for now):
                  pure (remoteRelSplice, _hrBody respBody)

              -- NOTE: preserve headers (see test_response_headers_from_remote)
              let GQRespValue resData resErrors = E.joinResults initValue results
                  errors = VB.build resErrors
              case V.null errors of
                True  -> pure resData
                False -> throwError (err400 Unexpected "Joining results for remote relationships failed")
                                    {qeInternal = Just $ J.object ["errors" J..= JO.fromOrdered (JO.Array errors)]}
              -- pure $ encodeGQRespValue (E.joinResults initValue results)

          let mergedResp = foldM JO.safeUnion JO.empty resps
          case mergedResp of
            Left e -> throw400 UnexpectedPayload $ "could not merge data from results: " <> T.pack e
            Right r -> pure $ JO.toEncJSON $ JO.Object r

    E.ExOpMutation tx -> do
      -- log the graphql query
      L.unLogger logger $ QueryLog query Nothing reqId
      runLazyTx pgExecCtx Q.ReadWrite $ withUserInfo userInfo tx

    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"

  resp <- liftEither respE
  let !json = encodeGQResp $ GQSuccess $ encJToLBS resp
      telemQueryType = case resolvedOp of E.ExOpMutation{} -> Telem.Mutation ; _ -> Telem.Query
  return (telemTimeIO, telemQueryType, json)
