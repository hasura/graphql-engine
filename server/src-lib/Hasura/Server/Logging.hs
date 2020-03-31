-- This is taken from wai-logger and customised for our use

module Hasura.Server.Logging
  ( StartupLog(..)
  , PGLog(..)
  , mkInconsMetadataLog
  , mkHttpAccessLogContext
  , mkHttpErrorLogContext
  , mkHttpLog
  , HttpInfoLog(..)
  , OperationLog(..)
  , HttpLogContext(..)
  , WebHookLog(..)
  , HttpException
  , getSourceFromFallback
  , getSource
  , HttpLog (..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Bits                 (shift, (.&.))
import           Data.ByteString.Char8     (ByteString)
import           Data.Int                  (Int64)
import           Data.List                 (find)
import           Data.Word                 (Word32)
import           Network.Socket            (SockAddr (..))
import           System.ByteOrder          (ByteOrder (..), byteOrder)
import           Text.Printf               (printf)

import qualified Data.ByteString.Char8     as BS
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Network.HTTP.Types        as HTTP
import qualified Network.Wai               as Wai

import           Hasura.HTTP
import           Hasura.Logging
import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.Server.Compression
import           Hasura.Server.Utils
import           Hasura.User

data StartupLog
  = StartupLog
  { slLogLevel :: !LogLevel
  , slKind     :: !T.Text
  , slInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON StartupLog where
  toJSON (StartupLog _ k info) =
    object [ "kind" .= k
           , "info" .= info
           ]

instance ToEngineLog StartupLog Hasura where
  toEngineLog startupLog =
    (slLogLevel startupLog, ELTStartup, toJSON startupLog)

data PGLog
  = PGLog
  { plLogLevel :: !LogLevel
  , plMessage  :: !T.Text
  } deriving (Show, Eq)

instance ToJSON PGLog where
  toJSON (PGLog _ msg) =
    object ["message" .= msg]

instance ToEngineLog PGLog Hasura where
  toEngineLog pgLog =
    (plLogLevel pgLog, ELTInternal ILTPgClient, toJSON pgLog)

data MetadataLog
  = MetadataLog
  { mlLogLevel :: !LogLevel
  , mlMessage  :: !T.Text
  , mlInfo     :: !Value
  } deriving (Show, Eq)

instance ToJSON MetadataLog where
  toJSON (MetadataLog _ msg infoVal) =
    object [ "message" .= msg
           , "info" .= infoVal
           ]

instance ToEngineLog MetadataLog Hasura where
  toEngineLog ml =
    (mlLogLevel ml, ELTInternal ILTMetadata, toJSON ml)

mkInconsMetadataLog :: [InconsistentMetadata] -> MetadataLog
mkInconsMetadataLog objs =
  MetadataLog LevelWarn "Inconsistent Metadata!" $
    object [ "objects" .= objs]

data WebHookLog
  = WebHookLog
  { whlLogLevel   :: !LogLevel
  , whlStatusCode :: !(Maybe HTTP.Status)
  , whlUrl        :: !T.Text
  , whlMethod     :: !HTTP.StdMethod
  , whlError      :: !(Maybe HttpException)
  , whlResponse   :: !(Maybe T.Text)
  } deriving (Show)

instance ToEngineLog WebHookLog Hasura where
  toEngineLog webHookLog =
    (whlLogLevel webHookLog, ELTWebhookLog, toJSON webHookLog)

instance ToJSON WebHookLog where
  toJSON whl =
    object [ "status_code" .= (HTTP.statusCode <$> whlStatusCode whl)
           , "url" .= whlUrl whl
           , "method" .= show (whlMethod whl)
           , "http_error" .= whlError whl
           , "response" .= whlResponse whl
           ]


class (Monad m) => HttpLog m where
  logHttpError
    :: Logger Hasura
    -- ^ the logger
    -> Maybe UserInfo
    -- ^ user info may or may not be present (error can happen during user resolution)
    -> RequestId
    -- ^ request id of the request
    -> Wai.Request
    -- ^ the Wai.Request object
    -> Either BL.ByteString Value
    -- ^ the actual request body (bytestring if unparsed, Aeson value if parsed)
    -> QErr
    -- ^ the error
    -> [HTTP.Header]
    -- ^ list of request headers
    -> m ()

  logHttpSuccess
    :: Logger Hasura
    -- ^ the logger
    -> Maybe UserInfo
    -- ^ user info may or may not be present (error can happen during user resolution)
    -> RequestId
    -- ^ request id of the request
    -> Wai.Request
    -- ^ the Wai.Request object
    -> Maybe Value
    -- ^ the actual request body, if present
    -> BL.ByteString
    -- ^ the response bytes
    -> BL.ByteString
    -- ^ the compressed response bytes
    -- ^ TODO: make the above two type represented
    -> Maybe (DiffTime, DiffTime)
    -- ^ IO/network wait time and service time (respectively) for this request, if available.
    -> Maybe CompressionType
    -- ^ possible compression type
    -> [HTTP.Header]
    -- ^ list of request headers
    -> m ()


-- | Log information about the HTTP request
data HttpInfoLog
  = HttpInfoLog
  { hlStatus      :: !HTTP.Status
  , hlMethod      :: !T.Text
  , hlSource      :: !T.Text
  , hlPath        :: !T.Text
  , hlHttpVersion :: !HTTP.HttpVersion
  , hlCompression :: !(Maybe CompressionType)
  , hlHeaders     :: ![HTTP.Header]
  -- ^ all the request headers
  } deriving (Show, Eq)

instance ToJSON HttpInfoLog where
  toJSON (HttpInfoLog st met src path hv compressTypeM _) =
    object [ "status" .= HTTP.statusCode st
           , "method" .= met
           , "ip" .= src
           , "url" .= path
           , "http_version" .= show hv
           , "content_encoding" .= (compressionTypeToTxt <$> compressTypeM)
           ]

-- | Information about a GraphQL/Hasura metadata operation over HTTP
data OperationLog
  = OperationLog
  { olRequestId          :: !RequestId
  , olUserVars           :: !(Maybe SessionVariables)
  , olResponseSize       :: !(Maybe Int64)
  , olRequestReadTime    :: !(Maybe Seconds)
  -- ^ Request IO wait time, i.e. time spent reading the full request from the socket.
  , olQueryExecutionTime :: !(Maybe Seconds)
  -- ^ Service time, not including request IO wait time.
  , olQuery              :: !(Maybe Value)
  , olRawQuery           :: !(Maybe Text)
  , olError              :: !(Maybe QErr)
  } deriving (Show, Eq)

$(deriveToJSON (aesonDrop 2 snakeCase)
  { omitNothingFields = True
  } ''OperationLog)

data HttpLogContext
  = HttpLogContext
  { hlcHttpInfo  :: !HttpInfoLog
  , hlcOperation :: !OperationLog
  } deriving (Show, Eq)
$(deriveToJSON (aesonDrop 3 snakeCase) ''HttpLogContext)

mkHttpAccessLogContext
  :: Maybe UserInfo
  -- ^ Maybe because it may not have been resolved
  -> RequestId
  -> Wai.Request
  -> BL.ByteString
  -> Maybe (DiffTime, DiffTime)
  -> Maybe CompressionType
  -> [HTTP.Header]
  -> HttpLogContext
mkHttpAccessLogContext userInfoM reqId req res mTiming compressTypeM headers =
  let http = HttpInfoLog
             { hlStatus      = status
             , hlMethod      = bsToTxt $ Wai.requestMethod req
             , hlSource      = bsToTxt $ getSourceFromFallback req
             , hlPath        = bsToTxt $ Wai.rawPathInfo req
             , hlHttpVersion = Wai.httpVersion req
             , hlCompression  = compressTypeM
             , hlHeaders     = headers
             }
      op = OperationLog
           { olRequestId    = reqId
           , olUserVars     = _uiSession <$> userInfoM
           , olResponseSize = respSize
           , olRequestReadTime    = Seconds . fst <$> mTiming
           , olQueryExecutionTime = Seconds . snd <$> mTiming
           , olQuery = Nothing
           , olRawQuery = Nothing
           , olError = Nothing
           }
  in HttpLogContext http op
  where
    status = HTTP.status200
    respSize = Just $ BL.length res

mkHttpErrorLogContext
  :: Maybe UserInfo
  -- ^ Maybe because it may not have been resolved
  -> RequestId
  -> Wai.Request
  -> QErr
  -> Either BL.ByteString Value
  -> Maybe (DiffTime, DiffTime)
  -> Maybe CompressionType
  -> [HTTP.Header]
  -> HttpLogContext
mkHttpErrorLogContext userInfoM reqId req err query mTiming compressTypeM headers =
  let http = HttpInfoLog
             { hlStatus      = qeStatus err
             , hlMethod      = bsToTxt $ Wai.requestMethod req
             , hlSource      = bsToTxt $ getSourceFromFallback req
             , hlPath        = bsToTxt $ Wai.rawPathInfo req
             , hlHttpVersion = Wai.httpVersion req
             , hlCompression  = compressTypeM
             , hlHeaders     = headers
             }
      op = OperationLog
           { olRequestId          = reqId
           , olUserVars           = _uiSession <$> userInfoM
           , olResponseSize       = Just $ BL.length $ encode err
           , olRequestReadTime    = Seconds . fst <$> mTiming
           , olQueryExecutionTime = Seconds . snd <$> mTiming
           , olQuery              = either (const Nothing) Just query
           , olRawQuery           = either (Just . bsToTxt . BL.toStrict) (const Nothing) query
           , olError              = Just err
           }
  in HttpLogContext http op

data HttpLogLine
  = HttpLogLine
  { _hlLogLevel :: !LogLevel
  , _hlLogLine  :: !HttpLogContext
  }

instance ToEngineLog HttpLogLine Hasura where
  toEngineLog (HttpLogLine logLevel logLine) =
    (logLevel, ELTHttpLog, toJSON logLine)

mkHttpLog :: HttpLogContext -> HttpLogLine
mkHttpLog httpLogCtx =
  let isError = isJust $ olError $ hlcOperation httpLogCtx
      logLevel = bool LevelInfo LevelError isError
  in HttpLogLine logLevel httpLogCtx

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
