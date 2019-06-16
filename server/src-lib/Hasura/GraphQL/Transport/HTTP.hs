module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as N

import           Hasura.EncJSON
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Context

import qualified Hasura.GraphQL.Execute as E

runGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> UserInfo
  -> SQLGenCtx
  -> Bool
  -> E.PlanCache
  -> SchemaCache
  -> SchemaCacheVer
  -> HTTP.Manager
  -> [N.Header]
  -> GQLReqUnparsed
  -> m (HttpResponse EncJSON)
runGQ pgExecCtx userInfo sqlGenCtx enableAL planCache sc scVer manager reqHdrs req = do
  execPlans <-
    E.getResolvedExecPlan
      pgExecCtx
      planCache
      userInfo
      sqlGenCtx
      enableAL
      sc
      scVer
      req
  results <-
    forM execPlans $ \execPlan ->
      case execPlan of
        E.ExPHasura resolvedOp -> do
          encJson <- runHasuraGQ pgExecCtx userInfo resolvedOp
          pure (HttpResponse encJson Nothing)
        E.ExPRemote rsi -> E.execRemoteGQ manager userInfo reqHdrs rsi
        E.ExPMixed resolvedOp remoteRels -> do
          encJson <- runHasuraGQ pgExecCtx userInfo resolvedOp
          liftIO $ putStrLn ("hasura_JSON = " ++ show encJson)
          let result =
                E.extractRemoteRelArguments
                  (scRemoteResolvers sc)
                  encJson
                  remoteRels
          liftIO $ putStrLn ("extractRemoteRelArguments = " ++ show result)
          case result of
            Left e -> error e
            Right remotes -> do
              let batches = E.produceBatches remotes
              liftIO $ putStrLn ("batches = " ++ show batches)
              results <-
                traverse
                  (\(remoteTopQuery, path) ->
                     do HttpResponse result _ <- E.execRemoteGQ manager userInfo reqHdrs remoteTopQuery
                        liftIO (putStrLn ("remote result = " ++ show result))
                        pure result)
                  batches
              pure ()
          pure (HttpResponse encJson Nothing)
  case mergeResponseData (toList (fmap _hrBody results)) of
    Right merged -> do
      liftIO (putStrLn ("Response:\n" ++ L8.unpack (encJToLBS merged)))
      pure (HttpResponse merged (foldMap _hrHeaders results))
    Left err -> throw500 ("Invalid response: " <> T.pack err)

runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => PGExecCtx
  -> UserInfo
  -> E.ExecOp
  -> m EncJSON
runHasuraGQ pgExecCtx userInfo resolvedOp = do
  respE <- liftIO $ runExceptT $ case resolvedOp of
    E.ExOpQuery tx    ->
      runLazyTx' pgExecCtx tx
    E.ExOpMutation tx ->
      runLazyTx pgExecCtx $ withUserInfo userInfo tx
    E.ExOpSubs _ ->
      throw400 UnexpectedPayload
      "subscriptions are not supported over HTTP, use websockets instead"
  resp <- liftEither respE
  return $ encodeGQResp $ GQSuccess $ encJToLBS resp

-- | Merge the list of objects by the @data@ key.
-- TODO: Duplicate keys are ignored silently; handle this.
-- TODO: Original order of keys is not preserved, either.
mergeResponseData :: [EncJSON] -> Either String EncJSON
mergeResponseData =
  fmap (encJFromJValue . Object . HM.singleton "data" . Object . HM.unions) .
  traverse (parse . encJToLBS)
  where
    parse :: L.ByteString -> Either String (HashMap Text Value)
    parse = eitherDecode >=> getData
    getData ::
         HashMap Text (HashMap Text Value) -> Either String (HashMap Text Value)
    getData hm =
      case HM.lookup "data" hm of
        Nothing -> Left "No `data' key in response!"
        Just data' -> pure data'
