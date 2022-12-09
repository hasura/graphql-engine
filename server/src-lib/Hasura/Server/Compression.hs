module Hasura.Server.Compression
  ( compressResponse,
    CompressionType (..),
    EncodingType,
    identityEncoding,
    contentEncodingHeader,
    compressionTypeToTxt,
    compressFast,

    -- * exported for testing
    getAcceptedEncodings,
  )
where

import Codec.Compression.GZip qualified as GZ
import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import Data.Text qualified as T
import Hasura.Prelude
import Hasura.Server.Utils (gzipHeader)
import Network.HTTP.Types.Header qualified as NH

-- | Compressed encodings which hasura supports
data CompressionType
  = CTGZip
  deriving (Show, Eq, Ord)

-- | Accept-Encoding directives (from client) which hasura supports. @Nothing@
-- indicates identity (no compression)
type EncodingType = Maybe CompressionType

identityEncoding :: EncodingType
identityEncoding = Nothing

compressionTypeToTxt :: CompressionType -> Text
compressionTypeToTxt CTGZip = "gzip"

-- | A map from Accept-Encoding directives to corresponding Content-Encoding
-- headers (from server). NOTE: @identity@ is not a valid directive for this
-- header.
contentEncodingHeader :: CompressionType -> NH.Header
contentEncodingHeader CTGZip = gzipHeader

-- | Maybe compress the response body, based on the client's Accept-Encoding
-- and our own judgement.
compressResponse ::
  NH.RequestHeaders ->
  BL.ByteString ->
  -- | The response body (possibly compressed), and the encoding chosen
  (BL.ByteString, EncodingType)
compressResponse reqHeaders unCompressedResp
  -- we have option to gzip:
  | acceptedEncodings == Set.fromList [identityEncoding, Just CTGZip]
      ||
      -- we must gzip:
      acceptedEncodings == Set.fromList [Just CTGZip] =
      (compressFast CTGZip unCompressedResp, Just CTGZip)
  -- we must only return an uncompressed response:
  | acceptedEncodings == Set.fromList [identityEncoding] =
      (unCompressedResp, identityEncoding)
  -- this is technically a client error, but ignore for now (maintaining
  -- current behavior); assume identity:
  | otherwise =
      (unCompressedResp, identityEncoding)
  where
    acceptedEncodings = getAcceptedEncodings reqHeaders

-- | Compress using
compressFast :: CompressionType -> BL.ByteString -> BL.ByteString
compressFast = \case
  CTGZip -> GZ.compressWith gzipCompressionParams
  where
    gzipCompressionParams =
      -- See Note [Compression ratios]
      GZ.defaultCompressParams {GZ.compressLevel = GZ.compressionLevel 1}

-- | Which encodings can the client accept? The empty set returned here is an
-- error condition and the server tecnically ought to return a 406.
--
-- https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Accept-Encoding
getAcceptedEncodings :: NH.RequestHeaders -> Set.Set EncodingType
getAcceptedEncodings reqHeaders = Set.fromList acceptedEncodingTypes
  where
    rawHeaderVals =
      concatMap (splitHeaderVal . snd) $
        filter (\h -> fst h == NH.hAcceptEncoding) reqHeaders
    splitHeaderVal bs = map T.strip $ T.splitOn "," $ bsToTxt bs
    -- we'll ignore qvalues, except (crucially) to determine if 'identity' is rejected:
    identityRejected =
      -- ...if we're explicitly rejecting identity, or...
      "identity;q=0" `elem` rawHeaderVals
        ||
        -- ...rejecting anything not listed and identity is not listed
        ( "*;q=0" `elem` rawHeaderVals
            && (not $ any ("identity" `T.isPrefixOf`) rawHeaderVals)
        )
    gzipAccepted =
      any ("gzip" `T.isPrefixOf`) rawHeaderVals
        && ("gzip;q=0" `notElem` rawHeaderVals)
    -- AFAICT missing header, or *,  implies “send whatever you want”
    -- https://www.rfc-editor.org/rfc/rfc7231#section-5.3.4
    anyEncodingTechnicallyAcceptable =
      null rawHeaderVals || rawHeaderVals == ["*"]
    acceptedEncodingTypes
      -- \| anyEncodingTechnicallyAcceptable = [Just CTGZip, identityEncoding]
      -- NOTE!: For now to be conservative and maintain historical behavior we
      -- will treat this case as “only identity is acceptable”:
      | anyEncodingTechnicallyAcceptable = [identityEncoding]
      | otherwise =
          (guard gzipAccepted $> Just CTGZip)
            <> (guard (not identityRejected) $> identityEncoding)

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

UPDATE (12/5):
~~~~~~~~~~~~~

Some recent data on compression ratios for graphql responsed (here as:
compressed_size / uncompressed_size) taken from cloud:

Aggregate across all responses where uncompressed > 700 bytes:

    max:    0.891 (worst compression)
    p99:    0.658
    p95:    0.565
    p75:    0.467
    p50:    0.346
    min:    0.005 (best compression)

Aggregate across responses where uncompressed > 17K bytes (90th percentile):

    max:    0.773
    p99:    0.414
    p95:    0.304
    p75:    0.202
    p50:    0.172
    min:    0.005

-}
