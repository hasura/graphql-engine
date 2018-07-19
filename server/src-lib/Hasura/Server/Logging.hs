{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- This is taken from wai-logger and customised for our use

module Hasura.Server.Logging
       ( ServerLogger
       , withStdoutLogger
       , ServerLog(..)
       , LogDetail(..)
       , LogDetailG
       , getRequestHeader
       ) where

import           Control.Exception        (bracket)
import           Data.Aeson
import           Data.Bits                (shift, (.&.))
import           Data.ByteString.Char8    (ByteString)
import           Data.Int                 (Int64)
import           Data.List                (find)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy           as TL
import           Data.Time.Clock
import           Data.Word                (Word32)
import           Network.Socket           (SockAddr (..))
import           Network.Wai              (Request (..))
import           System.ByteOrder         (ByteOrder (..), byteOrder)
import           System.Log.FastLogger
import           Text.Printf              (printf)

import qualified Data.ByteString.Char8    as BS
import qualified Data.CaseInsensitive     as CI
import qualified Data.HashMap.Strict      as M
import qualified Network.HTTP.Types       as N

import           Hasura.Prelude
import           Hasura.Server.Utils


data ServerLog
  = ServerLog
  { slStatus         :: !N.Status
  , slMethod         :: !T.Text
  , slSource         :: !T.Text
  , slPath           :: !T.Text
  , slTimestamp      :: !T.Text
  , slHttpVersion    :: !N.HttpVersion
  , slDetail         :: !(Maybe Value)
  , slRequestId      :: !(Maybe T.Text)
  -- , slHasuraId       :: !(Maybe T.Text)
  , slHasuraRole     :: !(Maybe T.Text)
  , slHasuraMetadata :: !(Maybe Value)
  , slQueryHash      :: !(Maybe T.Text)
  , slResponseSize   :: !(Maybe Int64)
  , slResponseTime   :: !(Maybe T.Text)
  } deriving (Show, Eq)

instance ToJSON ServerLog where
  toJSON (ServerLog st met src path ts hv det reqId hRole hMd qh rs rt) =
    object [ "status" .= N.statusCode st
           , "method" .= met
           , "ip" .= src
           , "url" .= path
           , "timestamp" .= ts
           , "http_version" .= show hv
           , "detail" .= det
           , "request_id" .= reqId
           -- , "hasura_id" .= hId
           , "hasura_role" .= hRole
           , "hasura_metadata" .= hMd
           , "query_hash" .= qh
           , "response_size" .= rs
           -- , "response_time" .= rt
           , "query_execution_time" .= rt
           ]

data LogDetail
  = LogDetail
  { ldQuery :: !TL.Text
  , ldError :: !Value
  } deriving (Show, Eq)

instance ToJSON LogDetail where
  toJSON (LogDetail q e) =
    object [ "request"  .= q
           , "error" .= e
           ]

-- type ServerLogger = Request -> BL.ByteString -> Either QErr BL.ByteString -> IO ()
type ServerLogger r = Request -> r -> Maybe (UTCTime, UTCTime) -> IO ()

type LogDetailG r = Request -> r -> (N.Status, Maybe Value, Maybe T.Text, Maybe Int64)

withStdoutLogger :: LogDetailG r -> (ServerLogger r -> IO a) -> IO a
withStdoutLogger detailF appf =
  bracket setup teardown $ \(rlogger, _) -> appf rlogger
  where
    setup = do
      getter <- newTimeCache "%FT%T%z"
      lgrset <- newStdoutLoggerSet defaultBufSize
      let logger req env timeT = do
            zdata <- getter
            let serverLog = mkServerLog detailF zdata req env timeT
            pushLogStrLn lgrset $ toLogStr $ encode serverLog
            when (isJust $ slDetail serverLog) $ flushLogStr lgrset
          remover = rmLoggerSet lgrset
      return (logger, remover)
    teardown (_, remover) = void remover

mkServerLog
  :: LogDetailG r
  -> FormattedTime
  -> Request
  -> r
  -> Maybe (UTCTime, UTCTime)
  -> ServerLog
mkServerLog detailF tmstr req r mTimeT =
  ServerLog
  { slStatus      = status
  , slMethod      = decodeBS $ requestMethod req
  , slSource      = decodeBS $ getSourceFromFallback req
  , slPath        = decodeBS $ rawPathInfo req
  , slTimestamp   = decodeBS tmstr
  , slHttpVersion = httpVersion req
  , slDetail      = mDetail
  , slRequestId   = decodeBS <$> getRequestId req
  -- , slHasuraId    = decodeBS <$> getHasuraId req
  , slHasuraRole  = decodeBS <$> getHasuraRole req
  , slHasuraMetadata = getHasuraMetadata req
  , slResponseSize = size
  , slResponseTime = T.pack . show <$> diffTime
  , slQueryHash = queryHash
  }
  where
    (status, mDetail, queryHash, size) = detailF req r
    diffTime = case mTimeT of
      Nothing       -> Nothing
      Just (t1, t2) -> Just $ diffUTCTime t2 t1

decodeBS :: BS.ByteString -> T.Text
decodeBS = TE.decodeUtf8With TE.lenientDecode

getSourceFromSocket :: Request -> ByteString
getSourceFromSocket = BS.pack . showSockAddr . remoteHost

getSourceFromFallback :: Request -> ByteString
getSourceFromFallback req = fromMaybe (getSourceFromSocket req) $ getSource req

getSource :: Request -> Maybe ByteString
getSource req = addr
  where
    maddr = find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
    addr = fmap snd maddr
    hdrs = requestHeaders req

requestIdHeader :: T.Text
requestIdHeader = "x-request-id"

getRequestId :: Request -> Maybe ByteString
getRequestId = getRequestHeader $ TE.encodeUtf8 requestIdHeader

getHasuraRole :: Request -> Maybe ByteString
getHasuraRole = getRequestHeader $ TE.encodeUtf8 userRoleHeader

getRequestHeader :: ByteString -> Request -> Maybe ByteString
getRequestHeader hdrName req = snd <$> mHeader
  where
    mHeader = find (\h -> fst h == CI.mk hdrName) hdrs
    hdrs = requestHeaders req

newtype HasuraMetadata
  = HasuraMetadata { unHM :: M.HashMap T.Text T.Text } deriving (Show)

instance ToJSON HasuraMetadata where
  toJSON hash = toJSON $ M.fromList $ map (\(k,v) -> (format k, v)) hdrs
    where
      hdrs = M.toList $ unHM hash
      format = T.map underscorify . T.drop 2
      underscorify '-' = '_'
      underscorify c   = c

getHasuraMetadata :: Request -> Maybe Value
getHasuraMetadata req = case md of
  [] -> Nothing
  _  -> Just $ toJSON $ HasuraMetadata (M.fromList md)
  where
    md = filter filterFixedHeaders rawMd
    filterFixedHeaders (h,_) = h /= userRoleHeader && h /= accessKeyHeader
    rawMd = filter (\h -> "x-hasura-" `T.isInfixOf` fst h) hdrs
    hdrs = map hdrToTxt $ requestHeaders req
    hdrToTxt (k, v) = (T.toLower $ decodeBS $ CI.original k, decodeBS v)

-- |  A type for IP address in numeric string representation.
type NumericAddress = String

showIPv4 :: Word32 -> Bool -> NumericAddress
showIPv4 w32 little
    | little    = show b1 ++ "." ++ show b2 ++ "." ++ show b3 ++ "." ++ show b4
    | otherwise = show b4 ++ "." ++ show b3 ++ "." ++ show b2 ++ "." ++ show b1
  where
    t1 = w32
    t2 = shift t1 (-8)
    t3 = shift t2 (-8)
    t4 = shift t3 (-8)
    b1 = t1 .&. 0x000000ff
    b2 = t2 .&. 0x000000ff
    b3 = t3 .&. 0x000000ff
    b4 = t4 .&. 0x000000ff

showIPv6 :: (Word32,Word32,Word32,Word32) -> String
showIPv6 (w1,w2,w3,w4) =
    printf "%x:%x:%x:%x:%x:%x:%x:%x" s1 s2 s3 s4 s5 s6 s7 s8
  where
    (s1,s2) = split16 w1
    (s3,s4) = split16 w2
    (s5,s6) = split16 w3
    (s7,s8) = split16 w4
    split16 w = (h1,h2)
      where
        h1 = shift w (-16) .&. 0x0000ffff
        h2 = w .&. 0x0000ffff

-- | Convert 'SockAddr' to 'NumericAddress'. If the address is
--   IPv4-embedded IPv6 address, the IPv4 is extracted.
showSockAddr :: SockAddr -> NumericAddress
-- HostAddr is network byte order.
showSockAddr (SockAddrInet _ addr4)                       = showIPv4 addr4 (byteOrder == LittleEndian)
-- HostAddr6 is host byte order.
showSockAddr (SockAddrInet6 _ _ (0,0,0x0000ffff,addr4) _) = showIPv4 addr4 False
showSockAddr (SockAddrInet6 _ _ (0,0,0,1) _)              = "::1"
showSockAddr (SockAddrInet6 _ _ addr6 _)                  = showIPv6 addr6
showSockAddr _                                            = "unknownSocket"
