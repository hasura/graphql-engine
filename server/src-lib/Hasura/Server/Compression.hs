module Hasura.Server.Compression
  ( compressResponse
  , CompressionType(..)
  , compressionTypeToTxt
  )
where

import           Hasura.Prelude

import           Hasura.Server.Utils       (brHeader, gzipHeader)

import qualified Codec.Compression.Brotli  as BR
import qualified Codec.Compression.GZip    as GZ
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Network.HTTP.Types.Header as NH

data CompressionType
  = CTGZip
  | CTBrotli
  deriving (Show, Eq)

compressionTypeToTxt :: CompressionType -> T.Text
compressionTypeToTxt CTGZip   = "gzip"
compressionTypeToTxt CTBrotli = "brotli"

compressResponse
  :: NH.RequestHeaders
  -> BL.ByteString
  -> (BL.ByteString, Maybe (Text, Text), Maybe CompressionType)
compressResponse reqHeaders unCompressedResp =
  let compressionTypeM = getRequestedCompression reqHeaders
      appendCompressionType (res, headerM) = (res, headerM, compressionTypeM)
  in appendCompressionType $ case compressionTypeM of
       Just CTBrotli -> (BR.compress unCompressedResp, Just brHeader)
       Just CTGZip   -> (GZ.compress unCompressedResp, Just gzipHeader)
       Nothing       -> (unCompressedResp, Nothing)

getRequestedCompression :: NH.RequestHeaders -> Maybe CompressionType
getRequestedCompression reqHeaders
  | "br" `elem` acceptEncodingVals   = Just CTBrotli
  | "gzip" `elem` acceptEncodingVals = Just CTGZip
  | otherwise                        = Nothing
  where
    acceptEncodingVals = concatMap (splitHeaderVal . snd) $
                         filter (\h -> fst h == NH.hAcceptEncoding) reqHeaders
    splitHeaderVal bs = map T.strip $ T.splitOn "," $ bsToTxt bs
