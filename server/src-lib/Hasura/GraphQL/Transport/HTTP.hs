module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , runRemoteGQ
  ) where

import           Control.Exception                      (try)
import           Control.Lens

import qualified Data.ByteString.Lazy                   as BL
import qualified Data.CaseInsensitive                   as CI
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wreq                           as Wreq

import           Hasura.EncJSON
import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.Prelude
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Execute                 as E

runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool
  -> Q.TxIsolation
  -> E.QueryCache
  -> UserInfo
  -> SchemaCache
  -> HTTP.Manager
  -> [N.Header]
  -> GQLReqUnparsed
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m EncJSON
runGQ pool isoL queryCache userInfo sc manager reqHdrs reqUnparsed rawReq = do

  (gCtx, _) <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxRoleMap
  let schemaVer = scVersion sc

  (_, plan) <- E.getGQExecPlan queryCache userInfo schemaVer gCtx reqUnparsed
  case plan of
    E.GExPHasura opTy tx -> do
      resp <- case opTy of
        G.OperationTypeSubscription ->
          throw400 UnexpectedPayload
          "subscriptions are not supported over HTTP, use websockets instead"
        _ ->
          liftIO (runExceptT $ runTx $ liftTx tx) >>= liftEither
      return $ encodeGQResp $ GQSuccess resp
    E.GExPRemote rsi tod ->
      runRemoteGQ manager userInfo reqHdrs rawReq rsi tod

  where
    gCtxRoleMap = scGCtxMap sc
    runTx tx = runLazyTx pool isoL $ withUserInfo userInfo tx

runRemoteGQ
  :: (MonadIO m, MonadError QErr m)
  => HTTP.Manager
  -> UserInfo
  -> [N.Header]
  -> BL.ByteString
  -- ^ the raw request string
  -> RemoteSchemaInfo
  -> G.TypedOperationDefinition
  -> m EncJSON
runRemoteGQ manager userInfo reqHdrs q rsi opDef = do
  let opTy = G._todType opDef
  when (opTy == G.OperationTypeSubscription) $
    throw400 NotSupported "subscription to remote server is not supported"
  hdrs <- getHeadersFromConf hdrConf
  let confHdrs   = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) hdrs
      clientHdrs = bool [] filteredHeaders fwdClientHdrs
      options    = wreqOptions manager (userInfoToHdrs ++ clientHdrs ++ confHdrs)

  res  <- liftIO $ try $ Wreq.postWith options (show url) q
  resp <- either httpThrow return res
  return $ encJFromLBS $ resp ^. Wreq.responseBody

  where
    RemoteSchemaInfo url hdrConf fwdClientHdrs = rsi
    httpThrow :: (MonadError QErr m) => HTTP.HttpException -> m a
    httpThrow err = throw500 $ T.pack . show $ err

    userInfoToHdrs = map (\(k, v) -> (CI.mk $ CS.cs k, CS.cs v)) $
                 userInfoToList userInfo
    filteredHeaders = flip filter reqHdrs $ \(n, _) ->
      n `notElem` [ "Content-Length", "Content-MD5", "User-Agent", "Host"
                  , "Origin", "Referer" , "Accept", "Accept-Encoding"
                  , "Accept-Language", "Accept-Datetime"
                  , "Cache-Control", "Connection", "DNT"
                  ]
