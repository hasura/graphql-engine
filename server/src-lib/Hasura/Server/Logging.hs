-- This is taken from wai-logger and customised for our use
{-# LANGUAGE OverloadedStrings #-}

module Hasura.Server.Logging
  ( StartupLog(..)
  , PGLog(..)
  , mkInconsMetadataLog
  , mkAccessLog
  , getRequestHeader
  , WebHookLog(..)
  , WebHookLogger
  , HttpException
  , VerboseLogging(..)
  ) where

import           Data.Aeson
import           Data.Bits             (shift, (.&.))
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy  as BL
import           Data.Int              (Int64)
import           Data.List             (find)
import qualified Data.TByteString      as TBS
import qualified Data.Text             as T
import           Data.Time.Clock
import           Data.Word             (Word32)
import           Network.Socket        (SockAddr (..))
import           Network.Wai           (Request (..))
import           System.ByteOrder      (ByteOrder (..), byteOrder)
import           Text.Printf           (printf)

import qualified Data.Aeson            as J
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive  as CI
import qualified Network.HTTP.Types    as N

import           Hasura.HTTP
import qualified Hasura.Logging        as L
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils


newtype VerboseLogging
  = VerboseLogging { unVerboseLogging :: Bool }
  deriving (Show, Eq, Generic, J.ToJSON)

data StartupLog
  = StartupLog
  { slLogLevel :: !L.LogLevel
  , slKind     :: !T.Text
  , slInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON StartupLog where
  toJSON (StartupLog _ k info) =
    object [ "kind" .= k
           , "info" .= info
           ]

instance L.ToEngineLog StartupLog where
  toEngineLog startupLog =
    (slLogLevel startupLog, "startup", toJSON startupLog)

data PGLog
  = PGLog
  { plLogLevel :: !L.LogLevel
  , plMessage  :: !T.Text
  } deriving (Show, Eq)

instance ToJSON PGLog where
  toJSON (PGLog _ msg) =
    object ["message" .= msg]

instance L.ToEngineLog PGLog where
  toEngineLog pgLog =
    (plLogLevel pgLog, "pg-client", toJSON pgLog)

data MetadataLog
  = MetadataLog
  { mlLogLevel :: !L.LogLevel
  , mlMessage  :: !T.Text
  , mlInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON MetadataLog where
  toJSON (MetadataLog _ msg infoVal) =
    object [ "message" .= msg
           , "info" .= infoVal
           ]

instance L.ToEngineLog MetadataLog where
  toEngineLog ml =
    (mlLogLevel ml, "metadata", toJSON ml)

mkInconsMetadataLog :: [InconsistentMetadataObj] -> MetadataLog
mkInconsMetadataLog objs =
  MetadataLog L.LevelWarn "Inconsistent Metadata!" $
    object [ "objects" .= objs]

data WebHookLog
  = WebHookLog
  { whlLogLevel   :: !L.LogLevel
  , whlStatusCode :: !(Maybe N.Status)
  , whlUrl        :: !T.Text
  , whlMethod     :: !N.StdMethod
  , whlError      :: !(Maybe HttpException)
  , whlResponse   :: !(Maybe T.Text)
  } deriving (Show)

instance L.ToEngineLog WebHookLog where
  toEngineLog webHookLog =
    (whlLogLevel webHookLog, "webhook-log", toJSON webHookLog)

instance ToJSON WebHookLog where
  toJSON whl =
    object [ "status_code" .= (N.statusCode <$> whlStatusCode whl)
           , "url" .= whlUrl whl
           , "method" .= show (whlMethod whl)
           , "http_error" .= whlError whl
           , "response" .= whlResponse whl
           ]

type WebHookLogger = WebHookLog -> IO ()

data AccessLog
  = AccessLog
  { alStatus       :: !N.Status
  , alMethod       :: !T.Text
  , alSource       :: !T.Text
  , alPath         :: !T.Text
  , alHttpVersion  :: !N.HttpVersion
  , alDetail       :: !(Maybe Value)
  , alRequestId    :: !(Maybe T.Text)
  , alHasuraUser   :: !(Maybe UserVars)
  -- , alQueryHash    :: !(Maybe T.Text)
  , alResponseSize :: !(Maybe Int64)
  , alResponseTime :: !(Maybe Double)
  } deriving (Show, Eq)

instance L.ToEngineLog AccessLog where
  toEngineLog accessLog =
    (L.LevelInfo, "http-log", toJSON accessLog)

instance ToJSON AccessLog where
  toJSON (AccessLog st met src path hv det reqId hUser rs rt) =
    object [ "status" .= N.statusCode st
           , "method" .= met
           , "ip" .= src
           , "url" .= path
           , "http_version" .= show hv
           , "detail" .= det
           , "request_id" .= reqId
           , "user" .= hUser
           -- , "query_hash" .= qh
           , "response_size" .= rs
           , "query_execution_time" .= rt
           ]

data LogDetail
  = LogDetail
  { _ldQuery        :: !TBS.TByteString
  , _ldError        :: !(Maybe Value)
  , _ldGeneratedSql :: !(Maybe Text)
  } deriving (Show, Eq)

instance ToJSON LogDetail where
  toJSON (LogDetail q e sql) =
    object [ "request"  .= q
           , "error" .= e
           , "sql" .= sql
           ]

ravenLogGen
  :: VerboseLogging
  -> (BL.ByteString, Either QErr BL.ByteString)
  -- -> SchemaCache -> SQLGenCtx -> Maybe UserInfo
  -> (N.Status, Maybe Value, Maybe Int64)
ravenLogGen verLog (reqBody, res) =
  (status, toJSON <$> logDetail, Just size)
  where
    status = either qeStatus (const N.status200) res
    logDetail = either (Just . qErrToLogDetail) (const logVerbose) res
    reqBodyTxt = TBS.fromLBS reqBody
    qErrToLogDetail qErr =
      LogDetail reqBodyTxt (Just $ toJSON qErr) Nothing

    size = BL.length $ either encode id res

    logVerbose = if unVerboseLogging verLog
                 then Just $ LogDetail reqBodyTxt Nothing genedSql
                 else Nothing

    genedSql :: Maybe Text
    genedSql = Nothing

mkAccessLog
  :: VerboseLogging
  -> Maybe UserInfo -- may not have been resolved
  -> Request
  -> (BL.ByteString, Either QErr BL.ByteString)
  -> Maybe (UTCTime, UTCTime)
  -> AccessLog
mkAccessLog verLog userInfoM req r mTimeT =
  AccessLog
  { alStatus       = status
  , alMethod       = bsToTxt $ requestMethod req
  , alSource       = bsToTxt $ getSourceFromFallback req
  , alPath         = bsToTxt $ rawPathInfo req
  , alHttpVersion  = httpVersion req
  , alDetail       = mDetail
  , alRequestId    = bsToTxt <$> getRequestId req
  , alHasuraUser   = userVars <$> userInfoM
  , alResponseSize = size
  , alResponseTime = diffTime
  -- , alQueryHash    = queryHash
  }
  where
    (status, mDetail, size) = ravenLogGen verLog r
    diffTime = fmap (realToFrac . uncurry diffUTCTime) mTimeT

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
getRequestId = getRequestHeader $ txtToBs requestIdHeader

getRequestHeader :: ByteString -> Request -> Maybe ByteString
getRequestHeader hdrName req = snd <$> mHeader
  where
    mHeader = find (\h -> fst h == CI.mk hdrName) hdrs
    hdrs = requestHeaders req

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
