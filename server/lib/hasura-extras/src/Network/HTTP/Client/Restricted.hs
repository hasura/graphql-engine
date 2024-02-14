-- | Restricted `ManagerSettings` for <https://haskell-lang.org/library/http-client>
-- -
-- - Portions from http-client-tls Copyright (c) 2013 Michael Snoyman
-- - Portions from http-client-restricted Copyright 2018 Joey Hess <id@joeyh.name>
-- -
-- - License: MIT
module Network.HTTP.Client.Restricted
  ( Decision (..),
    Restriction,
    mkRestrictedManagerSettings,
    ConnectionRestricted (..),
  )
where

import Control.Exception
import Data.Default
import Data.Maybe
import Data.Typeable
import Hasura.Prelude (onNothing)
import Network.BSD (getProtocolNumber)
import Network.Connection qualified as NC
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.Internal qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.Socket
import Prelude

data Decision = Allow | Deny

type Restriction = AddrInfo -> Decision

-- | Blocked requests raise this exception, wrapped as 'InternalException'.
data ConnectionRestricted = ConnectionRestricted
  { crHostName :: String,
    crAddress :: AddrInfo
  }
  deriving (Show, Typeable)

instance Exception ConnectionRestricted

-- | Adjusts a ManagerSettings to enforce a Restriction. The restriction
-- will be checked each time a Request is made, and for each redirect
-- followed.
--
-- This overrides the `managerRawConnection`
-- and `managerTlsConnection` with its own implementations that check
-- the Restriction. They should otherwise behave the same as the
-- ones provided by http-client-tls.
--
-- This function is not exported, because using it with a ManagerSettings
-- produced by something other than http-client-tls would result in
-- surprising behavior, since its connection methods would not be used.
restrictManagerSettings ::
  Maybe NC.ConnectionContext ->
  Maybe NC.TLSSettings ->
  Restriction ->
  HTTP.ManagerSettings ->
  HTTP.ManagerSettings
restrictManagerSettings mcontext mtls cfg base =
  base
    { HTTP.managerRawConnection = restrictedRawConnection cfg,
      HTTP.managerTlsConnection = restrictedTlsConnection mcontext mtls cfg,
      HTTP.managerWrapException = wrapOurExceptions base
    }

-- | Makes a TLS-capable ManagerSettings with a Restriction applied to it.
--
-- The Restriction will be checked each time a Request is made, and for
-- each redirect followed.
--
-- Aside from checking the Restriction, it should behave the same as
-- `Network.HTTP.Client.TLS.mkManagerSettingsContext`
-- from http-client-tls.
--
-- > main = do
-- > 	manager <- newManager $ mkRestrictedManagerSettings myRestriction Nothing Nothing
-- >	request <- parseRequest "http://httpbin.org/get"
-- > 	response <- httpLbs request manager
-- > 	print $ responseBody response
--
-- See `mkManagerSettingsContext` for why
-- it can be useful to provide a `NC.ConnectionContext`.
--
-- Note that SOCKS is not supported.
mkRestrictedManagerSettings ::
  Restriction ->
  Maybe NC.ConnectionContext ->
  Maybe NC.TLSSettings ->
  HTTP.ManagerSettings
mkRestrictedManagerSettings cfg mcontext mtls =
  restrictManagerSettings mcontext mtls cfg $
    HTTP.mkManagerSettingsContext mcontext (fromMaybe def mtls) Nothing

wrapOurExceptions :: HTTP.ManagerSettings -> HTTP.Request -> IO a -> IO a
wrapOurExceptions base req a =
  let wrapper se
        | Just (_ :: ConnectionRestricted) <- fromException se =
            toException $
              HTTP.HttpExceptionRequest req $
                HTTP.InternalException se
        | otherwise = se
   in HTTP.managerWrapException base req (handle (throwIO . wrapper) a)

restrictedRawConnection :: Restriction -> IO (Maybe HostAddress -> String -> Int -> IO HTTP.Connection)
restrictedRawConnection cfg = getConnection cfg Nothing Nothing

restrictedTlsConnection :: Maybe NC.ConnectionContext -> Maybe NC.TLSSettings -> Restriction -> IO (Maybe HostAddress -> String -> Int -> IO HTTP.Connection)
restrictedTlsConnection mcontext mtls cfg =
  getConnection cfg (Just (fromMaybe def mtls)) mcontext

-- Based on Network.HTTP.Client.TLS.getTlsConnection.
--
-- Checks the Restriction
--
-- Does not support SOCKS.
getConnection ::
  Restriction ->
  Maybe NC.TLSSettings ->
  Maybe NC.ConnectionContext ->
  IO (Maybe HostAddress -> String -> Int -> IO HTTP.Connection)
getConnection restriction tls mcontext = do
  context <- onNothing mcontext NC.initConnectionContext
  return $ \_hostAddress hostName port ->
    bracketOnError
      (go context hostName port)
      NC.connectionClose
      convertConnection
  where
    go context hostName port = do
      let connparams =
            NC.ConnectionParams
              { NC.connectionHostname = host,
                NC.connectionPort = fromIntegral port,
                NC.connectionUseSecure = tls,
                NC.connectionUseSocks = Nothing -- unsupprted
              }
      proto <- getProtocolNumber "tcp"
      let serv = show port
      let hints =
            defaultHints
              { addrFlags = [AI_ADDRCONFIG],
                addrProtocol = proto,
                addrSocketType = Stream
              }
      addrs <- getAddrInfo (Just hints) (Just host) (Just serv)
      bracketOnError
        (firstSuccessful $ map tryToConnect addrs)
        close
        (\sock -> NC.connectFromSocket context sock connparams)
      where
        host = HTTP.strippedHostName hostName
        tryToConnect addr = case restriction addr of
          Allow ->
            bracketOnError
              (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
              close
              (\sock -> connect sock (addrAddress addr) >> return sock)
          Deny -> throwIO $ ConnectionRestricted host addr
        firstSuccessful [] = throwIO $ NC.HostNotResolved host
        firstSuccessful (a : as) =
          a `catch` \(e :: IOException) ->
            case as of
              [] -> throwIO e
              _ -> firstSuccessful as

-- Copied from Network.HTTP.Client.TLS, unfortunately not exported.
convertConnection :: NC.Connection -> IO HTTP.Connection
convertConnection conn =
  HTTP.makeConnection
    (NC.connectionGetChunk conn)
    (NC.connectionPut conn)
    -- Closing an SSL connection gracefully involves writing/reading
    -- on the socket.  But when this is called the socket might be
    -- already closed, and we get a @ResourceVanished@.
    (NC.connectionClose conn `Control.Exception.catch` \(_ :: IOException) -> return ())
