-- This is taken from wai-logger and customised for our use

module Hasura.Server.Logging
  ( StartupLog(..)
  , PGLog(..)
  , mkInconsMetadataLog
  , mkHttpAccessLog
  , mkHttpErrorLog
  , WebHookLog(..)
  , WebHookLogger
  , HttpException
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Bits             (shift, (.&.))
import           Data.ByteString.Char8 (ByteString)
import           Data.Int              (Int64)
import           Data.List             (find)
import           Data.Time.Clock
import           Data.Word             (Word32)
import           Network.Socket        (SockAddr (..))
import           System.ByteOrder      (ByteOrder (..), byteOrder)
import           Text.Printf           (printf)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text             as T
import qualified Network.HTTP.Types    as N
import qualified Network.Wai           as Wai

import           Hasura.HTTP
import           Hasura.Logging        (EngineLogType (..))
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Utils

import qualified Hasura.Logging        as L

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
    (slLogLevel startupLog, ELTStartup, toJSON startupLog)

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
    (plLogLevel pgLog, ELTPgClient, toJSON pgLog)

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
    (mlLogLevel ml, ELTMetadata, toJSON ml)

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
    (whlLogLevel webHookLog, ELTWebhookLog, toJSON webHookLog)

instance ToJSON WebHookLog where
  toJSON whl =
    object [ "status_code" .= (N.statusCode <$> whlStatusCode whl)
           , "url" .= whlUrl whl
           , "method" .= show (whlMethod whl)
           , "http_error" .= whlError whl
           , "response" .= whlResponse whl
           ]

type WebHookLogger = WebHookLog -> IO ()

-- | Log information about the HTTP request
data HttpInfoLog
  = HttpInfoLog
  { hlStatus      :: !N.Status
  , hlMethod      :: !T.Text
  , hlSource      :: !T.Text
  , hlPath        :: !T.Text
  , hlHttpVersion :: !N.HttpVersion
  } deriving (Show, Eq)

instance ToJSON HttpInfoLog where
  toJSON (HttpInfoLog st met src path hv) =
    object [ "status" .= N.statusCode st
           , "method" .= met
           , "ip" .= src
           , "url" .= path
           , "http_version" .= show hv
           ]

-- | Information about a GraphQL/Hasura metadata operation over HTTP
data OperationLog
  = OperationLog
  { olRequestId          :: !RequestId
  , olUserVars           :: !(Maybe UserVars)
  , olResponseSize       :: !(Maybe Int64)
  , olQueryExecutionTime :: !(Maybe Double)
  , olQuery              :: !(Maybe Value)
  , olError              :: !(Maybe Value)
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 2 snakeCase) ''OperationLog)

data HttpAccessLog
  = HttpAccessLog
  { halHttpInfo  :: !HttpInfoLog
  , halOperation :: !OperationLog
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 3 snakeCase) ''HttpAccessLog)

data HttpLog
  = HttpLog
  { _hlLogLevel :: !L.LogLevel
  , _hlLogLing  :: !HttpAccessLog
  }

instance L.ToEngineLog HttpLog where
  toEngineLog (HttpLog logLevel accessLog) =
    (logLevel, ELTHttpLog, toJSON accessLog)

mkHttpAccessLog
  :: Maybe UserInfo -- may not have been resolved
  -> RequestId
  -> Wai.Request
  -> BL.ByteString
  -> Maybe (UTCTime, UTCTime)
  -> HttpLog
mkHttpAccessLog userInfoM reqId req res mTimeT =
  let http = HttpInfoLog
             { hlStatus       = status
             , hlMethod       = bsToTxt $ Wai.requestMethod req
             , hlSource       = bsToTxt $ getSourceFromFallback req
             , hlPath         = bsToTxt $ Wai.rawPathInfo req
             , hlHttpVersion  = Wai.httpVersion req
             }
      op = OperationLog
           { olRequestId    = reqId
           , olUserVars     = userVars <$> userInfoM
           , olResponseSize = respSize
           , olQueryExecutionTime = respTime
           , olQuery = Nothing
           , olError = Nothing
           }
  in HttpLog L.LevelInfo $ HttpAccessLog http op
  where
    status = N.status200
    respSize = Just $ BL.length res
    respTime = computeTimeDiff mTimeT

mkHttpErrorLog
  :: Maybe UserInfo -- may not have been resolved
  -> RequestId
  -> Wai.Request
  -> QErr
  -> Maybe Value
  -> Maybe (UTCTime, UTCTime)
  -> HttpLog
mkHttpErrorLog userInfoM reqId req err query mTimeT =
  let http = HttpInfoLog
             { hlStatus       = status
             , hlMethod       = bsToTxt $ Wai.requestMethod req
             , hlSource       = bsToTxt $ getSourceFromFallback req
             , hlPath         = bsToTxt $ Wai.rawPathInfo req
             , hlHttpVersion  = Wai.httpVersion req
             }
      op = OperationLog
           { olRequestId    = reqId
           , olUserVars     = userVars <$> userInfoM
           , olResponseSize = respSize
           , olQueryExecutionTime = respTime
           , olQuery = toJSON <$> query
           , olError = Just $ toJSON err
           }
  in HttpLog L.LevelError $ HttpAccessLog http op
  where
    status = qeStatus err
    respSize = Just $ BL.length $ encode err
    respTime = computeTimeDiff mTimeT

computeTimeDiff :: Maybe (UTCTime, UTCTime) -> Maybe Double
computeTimeDiff = fmap (realToFrac . uncurry (flip diffUTCTime))

getSourceFromSocket :: Wai.Request -> ByteString
getSourceFromSocket = BS.pack . showSockAddr . Wai.remoteHost

getSourceFromFallback :: Wai.Request -> ByteString
getSourceFromFallback req = fromMaybe (getSourceFromSocket req) $ getSource req

getSource :: Wai.Request -> Maybe ByteString
getSource req = addr
  where
    maddr = find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
    addr = fmap snd maddr
    hdrs = Wai.requestHeaders req

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
