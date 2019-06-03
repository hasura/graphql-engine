module Hasura.GraphQL.Transport.WebSocket.Client
  ( runGqlClient
  , sendStopMsg
  , stopRemote
  , closeRemote
  ) where

import           Control.Concurrent                            (ThreadId,
                                                                forkIO,
                                                                killThread,
                                                                myThreadId)
import           Control.Exception                             (SomeException,
                                                                try)

import           Hasura.GraphQL.Transport.WebSocket.Connection
import           Hasura.GraphQL.Transport.WebSocket.Protocol   (OperationId (..))
import           Hasura.Prelude
import           Hasura.RQL.Types

import qualified Control.Concurrent.STM                        as STM
import qualified Data.Aeson                                    as J
import qualified Data.ByteString.Lazy                          as BL
import qualified Data.Text                                     as T
import qualified ListT
import qualified Network.URI                                   as URI
import qualified Network.WebSockets                            as WS
import qualified StmContainers.Map                             as STMMap

import qualified Hasura.GraphQL.Transport.WebSocket.Protocol   as WS
import qualified Hasura.GraphQL.Transport.WebSocket.Server     as WS
import qualified Hasura.Logging                                as L


sendInit :: WS.Connection -> Maybe WS.ConnParams -> IO ()
sendInit conn reqHdrs =
  WS.sendTextData conn $ J.encode (WS.CMConnInit reqHdrs)

sendStopMsg :: WS.Connection -> WS.StopMsg -> IO ()
sendStopMsg conn msg =
  WS.sendTextData conn $ J.encode $ WS.CMStop msg

mkGraphqlProxy
  :: L.Logger
  -> WS.WSConn a
  -> OperationMap
  -> RemoteSchemaName
  -> Maybe WS.ConnParams -- connection params in conn_init from the client
  -> OperationId
  -> WS.StartMsg -- the start msg
  -> ThreadId    -- The receive client threadId
  -> WS.ClientApp ()
mkGraphqlProxy logger wsConn opMap rn hdrs opId payload threadId destConn = do
  -- setup initial connection protocol
  setupInitialProto destConn
  let newState = RemoteOperation threadId destConn rn wsId
  STM.atomically $ STMMap.insert (SORemote newState) opId opMap
  -- setup the proxy from remote to hasura
  proxy destConn srcConn
  where
    wsId    = WS._wcConnId wsConn
    srcConn = WS._wcConnRaw wsConn

    -- function to receive data from the remote server and proxy it back to the
    -- hasura's client but work at a protocol level to also close the connection
    -- when a "complete" is encountered
    proxy recvConn sendConn =
      forever $ do
        msg <- WS.receiveData recvConn
        case J.eitherDecode' msg of
          Left e -> parseMsgErr e
          Right msgTy -> case msgTy of
            WS.SMT_GQL_CONNECTION_ACK        -> sendMsg sendConn msg
            WS.SMT_GQL_CONNECTION_KEEP_ALIVE -> sendMsg sendConn msg
            WS.SMT_GQL_CONNECTION_ERROR      -> sendMsg sendConn msg
            WS.SMT_GQL_DATA                  -> sendMsg sendConn msg
            WS.SMT_GQL_ERROR                 -> sendMsg sendConn msg
            WS.SMT_GQL_COMPLETE -> do
              sendMsg sendConn msg
              -- close this client connection
              killThread threadId

    sendMsg :: WS.Connection -> BL.ByteString -> IO ()
    sendMsg = WS.sendTextData

    -- send an init message with the same payload recieved, and then send the
    -- payload as it is (assuming this is the start msg)
    setupInitialProto conn = do
      sendInit conn hdrs
      WS.sendTextData conn $ J.encode $ WS.CMStart payload

    parseMsgErr err = do
      let opDet = ODQueryErr $ err400 UnexpectedPayload $ T.pack err
      L.unLogger logger $ WSLog wsId Nothing (EOperation opId Nothing opDet)
        (Just "failed to parse server msg from remote") Nothing

runGqlClient'
  :: L.Logger
  -> URI.URI
  -> WS.WSConn a
  -> OperationMap
  -> RemoteSchemaName
  -> OperationId
  -> Maybe WS.ConnParams
  -> WS.StartMsg
  -> ExceptT QErr IO ()
runGqlClient' logger url wsConn opMap rn opId hdrs payload = do
  host <- maybe (throw500 "empty hostname for websocket conn") return mHost
  void $ liftIO $ forkIO $ do
    tid <- myThreadId
    let gqClient = mkGraphqlProxy logger wsConn opMap rn hdrs opId payload tid
    res <- try $ WS.runClient host port path gqClient
    onLeft res $ \e -> do
      let err = T.pack $ show (e :: SomeException)
          opDet = ODQueryErr $ err500 Unexpected err
      L.unLogger logger $ WSLog wsId Nothing (EOperation opId Nothing opDet)
        (Just "exception from runClient thread") Nothing
  where
    uriAuth = URI.uriAuthority url
    path = URI.uriPath url
    mHost = (URI.uriUserInfo <$> uriAuth) <> (URI.uriRegName <$> uriAuth)
    port = let val = maybe "80" (drop 1 . URI.uriPort) uriAuth
           in read $ bool val "80" (null val)
    wsId = WS._wcConnId wsConn

findRemoteOps
  :: (MonadIO m)
  => OperationMap -> RemoteSchemaName -> m [RemoteOperation]
findRemoteOps opMap rn = do
  allOperations <- liftIO $ STM.atomically $ ListT.toList $ STMMap.listT opMap
  let remoteOps = mapMaybe (onlyRemotes . snd) allOperations
  return $ filter (\op -> _ropRemoteName op == rn) remoteOps
  where
    onlyRemotes = \case
      SORemote op -> Just op
      SOHasura _  -> Nothing

runGqlClient
  :: L.Logger
  -> URI.URI
  -> WS.WSConn a
  -> OperationMap
  -> RemoteSchemaName
  -> OperationId
  -> Maybe WS.ConnParams
  -> WS.StartMsg
  -> ExceptT QErr IO ()
runGqlClient logger url wsConn opMap rn opId hdrs startMsg = do
  remoteOps <- findRemoteOps opMap rn
  case remoteOps of
    []     -> runGqlClient' logger url wsConn opMap rn opId hdrs startMsg
    (op:_) -> do
      -- send the raw message on the existing conn
      let wsconn   = _ropRemoteConn op
          tid      = _ropRunClientThread op
          wsId     = WS._wcConnId wsConn
          remoteOp = RemoteOperation tid wsconn rn wsId
      liftIO $ do
        STM.atomically $ STMMap.insert (SORemote remoteOp) opId opMap
        -- send only start message on existing connection
        WS.sendTextData wsconn $ J.encode $ WS.CMStart startMsg


stopRemote
  :: L.Logger
  -> OperationMap
  -> OperationId
  -> RemoteOperation
  -> IO ()
stopRemote (L.Logger logger) opMap opId remoteOp = do
  liftIO $ logger $ L.debugT "stopping the remote.."
  let (RemoteOperation thrId wsConn rn _) = remoteOp
  -- TODO: should do this atomically?
  liftIO $ sendStopMsg wsConn $ WS.StopMsg opId
  -- remove the operation from the map
  STM.atomically $ STMMap.delete opId opMap
  -- remaing operations
  remoteOps <- findRemoteOps opMap rn
  when (null remoteOps) $ do
    -- close the client connection to remote
    liftIO $ logger $ L.debugT "no remaining ops; closing remote"
    liftIO $ killThread thrId


closeRemote :: L.Logger -> WS.WSId -> RemoteOperation -> IO ()
closeRemote (L.Logger logger) wsId remoteOp = do
  logger $ L.debugT $ "closing connections to: " <>
    unRemoteSchemaName (_ropRemoteName remoteOp)
  logger $ L.debugT $ "belonging to websocket id: " <> T.pack (show wsId)
  liftIO $ killThread $ _ropRunClientThread remoteOp
