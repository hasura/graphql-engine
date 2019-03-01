module Hasura.GraphQL.Transport.HTTP
  ( runGQ
  , getTopLevelNodes
  , gatherTypeLocs
  , assertSameLocationNodes
  , runRemoteGQ
  ) where

import           Control.Exception                      (try)
import           Control.Lens
import           Hasura.Prelude

import qualified Data.ByteString.Lazy                   as BL
import qualified Data.CaseInsensitive                   as CI
import qualified Data.HashMap.Strict                    as Map
import qualified Data.HashSet                           as Set
import qualified Data.String.Conversions                as CS
import qualified Data.Text                              as T
import qualified Database.PG.Query                      as Q
import qualified Language.GraphQL.Draft.Syntax          as G
import qualified Network.HTTP.Client                    as HTTP
import qualified Network.HTTP.Types                     as N
import qualified Network.Wreq                           as Wreq

import           Hasura.GraphQL.Schema
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.HTTP
import           Hasura.RQL.DDL.Headers
import           Hasura.RQL.Types

import qualified Hasura.GraphQL.Resolve                 as R
import qualified Hasura.GraphQL.Validate                as VQ
import qualified Hasura.GraphQL.Validate.Types          as VT


runGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> SchemaCache
  -> HTTP.Manager
  -> [N.Header]
  -> GraphQLRequest
  -> BL.ByteString -- this can be removed when we have a pretty-printer
  -> m BL.ByteString
runGQ pool isoL userInfo sc manager reqHdrs req rawReq = do

  (gCtx, _) <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxRoleMap
  queryParts <- flip runReaderT gCtx $ VQ.getQueryParts req

  let opDef = VQ.qpOpDef queryParts
      topLevelNodes = getTopLevelNodes opDef
      -- gather TypeLoc of topLevelNodes
      typeLocs = gatherTypeLocs gCtx topLevelNodes

  -- see if they are all the same
  assertSameLocationNodes typeLocs

  case typeLocs of
    [] -> runHasuraGQ pool isoL userInfo sc queryParts

    (typeLoc:_) -> case typeLoc of
      VT.HasuraType ->
        runHasuraGQ pool isoL userInfo sc queryParts
      VT.RemoteType _ rsi ->
        runRemoteGQ manager userInfo reqHdrs rawReq rsi opDef
  where
    gCtxRoleMap = scGCtxMap sc


assertSameLocationNodes :: (MonadError QErr m) => [VT.TypeLoc] -> m ()
assertSameLocationNodes typeLocs =
  unless (allEq typeLocs) $ throw400 NotSupported msg
  where
    allEq xs = case xs of
      [] -> True
      _  -> Set.size (Set.fromList xs) == 1
    msg = "cannot mix nodes from two different graphql servers"

-- TODO: we should retire the function asap
getTopLevelNodes :: G.TypedOperationDefinition -> [G.Name]
getTopLevelNodes opDef =
  mapMaybe f $ G._todSelectionSet opDef
  where
    -- TODO: this will fail when there is a fragment at the top level
    f = \case
      G.SelectionField fld        -> Just $ G._fName fld
      G.SelectionFragmentSpread _ -> Nothing
      G.SelectionInlineFragment _ -> Nothing

gatherTypeLocs :: GCtx -> [G.Name] -> [VT.TypeLoc]
gatherTypeLocs gCtx nodes =
  catMaybes $ flip map nodes $ \node ->
    VT._fiLoc <$> Map.lookup node schemaNodes
  where
    schemaNodes =
      let qr = VT._otiFields $ _gQueryRoot gCtx
          mr = VT._otiFields <$> _gMutRoot gCtx
      in maybe qr (Map.union qr) mr

runHasuraGQ
  :: (MonadIO m, MonadError QErr m)
  => Q.PGPool -> Q.TxIsolation
  -> UserInfo
  -> SchemaCache
  -> VQ.QueryParts
  -> m BL.ByteString
runHasuraGQ pool isoL userInfo sc queryParts = do
  (gCtx, _) <- flip runStateT sc $ getGCtx (userRole userInfo) gCtxMap
  (opTy, fields) <- runReaderT (VQ.validateGQ queryParts) gCtx
  when (opTy == G.OperationTypeSubscription) $ throw400 UnexpectedPayload
    "subscriptions are not supported over HTTP, use websockets instead"
  let tx = R.resolveSelSet userInfo gCtx opTy fields
  resp <- liftIO (runExceptT $ runTx tx) >>= liftEither
  return $ encodeGQResp $ GQSuccess resp
  where
    gCtxMap = scGCtxMap sc
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
  -> m BL.ByteString
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
  return $ resp ^. Wreq.responseBody

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
