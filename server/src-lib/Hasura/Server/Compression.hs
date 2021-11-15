module Hasura.Server.Compression
  ( compressResponse,
    CompressionType (..),
    compressionTypeToTxt,
  )
where

import Codec.Compression.GZip qualified as GZ
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Hasura.Prelude
import Hasura.Server.Utils (gzipHeader)
import Network.HTTP.Types.Header qualified as NH

data CompressionType
  = CTGZip
  deriving (Show, Eq)

compressionTypeToTxt :: CompressionType -> Text
compressionTypeToTxt CTGZip = "gzip"

compressResponse ::
  NH.RequestHeaders ->
  BL.ByteString ->
  (BL.ByteString, Maybe NH.Header, Maybe CompressionType)
compressResponse reqHeaders unCompressedResp =
  let compressionTypeM = getRequestedCompression reqHeaders
      appendCompressionType (res, headerM) = (res, headerM, compressionTypeM)
      gzipCompressionParams =
        GZ.defaultCompressParams {GZ.compressLevel = GZ.compressionLevel 1}
   in appendCompressionType $ case compressionTypeM of
        Just CTGZip -> (GZ.compressWith gzipCompressionParams unCompressedResp, Just gzipHeader)
        Nothing -> (unCompressedResp, Nothing)

getRequestedCompression :: NH.RequestHeaders -> Maybe CompressionType
getRequestedCompression reqHeaders
  | "gzip" `elem` acceptEncodingVals = Just CTGZip
  | otherwise = Nothing
  where
    acceptEncodingVals =
      concatMap (splitHeaderVal . snd) $
        filter (\h -> fst h == NH.hAcceptEncoding) reqHeaders
    splitHeaderVal bs = map T.strip $ T.splitOn "," $ bsToTxt bs
