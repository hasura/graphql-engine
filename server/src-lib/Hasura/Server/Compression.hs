module Hasura.Server.Compression
  ( compressResponse
  , CompressionType(..)
  , compressionTypeToTxt
  )
where

import           Hasura.Prelude

import           Hasura.Server.Utils       (gzipHeader)

import qualified Codec.Compression.GZip    as GZ
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Network.HTTP.Types.Header as NH

data CompressionType
  = CTGZip
  deriving (Show, Eq)

compressionTypeToTxt :: CompressionType -> T.Text
compressionTypeToTxt CTGZip   = "gzip"

compressResponse
  :: NH.RequestHeaders
  -> BL.ByteString
  -> (BL.ByteString, Maybe NH.Header, Maybe CompressionType)
compressResponse reqHeaders unCompressedResp =
  let compressionTypeM = getRequestedCompression reqHeaders
      appendCompressionType (res, headerM) = (res, headerM, compressionTypeM)
      gzipCompressionParams =
        GZ.defaultCompressParams{GZ.compressLevel = GZ.compressionLevel 1}
  in appendCompressionType $ case compressionTypeM of
       Just CTGZip -> (GZ.compressWith gzipCompressionParams unCompressedResp, Just gzipHeader)
       Nothing     -> (unCompressedResp, Nothing)

getRequestedCompression :: NH.RequestHeaders -> Maybe CompressionType
getRequestedCompression reqHeaders
  | "gzip" `elem` acceptEncodingVals = Just CTGZip
  | otherwise                        = Nothing
  where
    acceptEncodingVals = concatMap (splitHeaderVal . snd) $
                         filter (\h -> fst h == NH.hAcceptEncoding) reqHeaders
    splitHeaderVal bs = map T.strip $ T.splitOn "," $ bsToTxt bs
