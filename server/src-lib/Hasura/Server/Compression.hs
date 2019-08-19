module Hasura.Server.Compression
  (compressResponse)
where

import           Hasura.Prelude

import           Hasura.Server.Utils       (brHeader, gzipHeader)

import qualified Codec.Compression.Brotli  as BR
import qualified Codec.Compression.GZip    as GZ
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Network.HTTP.Types.Header as NH

data RequiredEncoding
  = REGZip
  | REBrotli
  | RENothing
  deriving (Show, Eq)

compressResponse
  :: Bool
  -> NH.RequestHeaders
  -> BL.ByteString
  -> (BL.ByteString, Maybe (Text, Text))
compressResponse False _ resp                         = (resp, Nothing)
compressResponse True requestHeaders unCompressedResp =
  case getRequiredEncoding requestHeaders of
    REGZip    -> (GZ.compress unCompressedResp, Just gzipHeader)
    REBrotli  -> (BR.compress unCompressedResp, Just brHeader)
    RENothing -> (unCompressedResp, Nothing)

getRequiredEncoding :: NH.RequestHeaders -> RequiredEncoding
getRequiredEncoding reqHeaders
  | "br" `elem` acceptEncodingVals   = REBrotli
  | "gzip" `elem` acceptEncodingVals = REGZip
  | otherwise                        = RENothing
  where
    acceptEncodingVals = concatMap (splitHeaderVal . snd) $
                         filter (\h -> fst h == NH.hAcceptEncoding) reqHeaders
    splitHeaderVal bs = map T.strip $ T.splitOn "," $ bsToTxt bs
