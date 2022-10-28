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

-- | Maybe compress the response body
compressResponse ::
  NH.RequestHeaders ->
  BL.ByteString ->
  (BL.ByteString, Maybe NH.Header, Maybe CompressionType)
compressResponse reqHeaders unCompressedResp =
  let compressionTypeM = getAcceptedCompression reqHeaders
      appendCompressionType (res, headerM) = (res, headerM, compressionTypeM)
      gzipCompressionParams =
        -- See Note [Compression ratios]
        GZ.defaultCompressParams {GZ.compressLevel = GZ.compressionLevel 1}
   in appendCompressionType $ case compressionTypeM of
        Just CTGZip -> (GZ.compressWith gzipCompressionParams unCompressedResp, Just gzipHeader)
        Nothing -> (unCompressedResp, Nothing)

-- | Which, if any, compressed encodings can the client accept?
--
-- https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
getAcceptedCompression :: NH.RequestHeaders -> Maybe CompressionType
getAcceptedCompression reqHeaders
  | "gzip" `elem` acceptEncodingVals = Just CTGZip
  | otherwise = Nothing
  where
    acceptEncodingVals =
      concatMap (splitHeaderVal . snd) $
        filter (\h -> fst h == NH.hAcceptEncoding) reqHeaders
    splitHeaderVal bs = map T.strip $ T.splitOn "," $ bsToTxt bs

{-
Note [Compression ratios]
~~~~~~~~~~~~~~~~~~~~~~~~~

I did some measurements of compression ratios at `gzip -1` (libc) of some
randomly generated json, real json datasets, and output from our benchmarked
chinook queries:

    2552/6131    = 0.41
    4666/8718    = 0.53
    13921/27131  = 0.51
    5895/8879    = 0.66  <----- completely random strings
    8634/28261   = 0.30
    70422/372466 = 0.18

    200/600      = 0.33  <----| from chinook graphql benchmarks
    3000/33000   = 0.09  <----|
    13000/190000 = 0.07  <----'

Given these numbers I would suggest using a rule-of-thumb expected compression
ratio between 2:1 and 10:1, depending on what being conservative means in the
context.

I didn't test higher compression levels much, but `gzip -4` for the most part
resulted in less than 10% smaller output on random json, and ~30% on our highly
compressible benchmark output.
-}
