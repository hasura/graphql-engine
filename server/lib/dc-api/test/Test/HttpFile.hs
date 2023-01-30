{-# LANGUAGE DeriveAnyClass #-}

module Test.HttpFile
  ( writeRequest,
    writeResponse,
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LazyBS
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy.Builder qualified as TextBuilder
import Data.Text.Lazy.Encoding qualified as LazyText
import Data.Text.Lazy.IO qualified as LazyText
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types (Header, Status (..))
import Servant.Client (BaseUrl, showBaseUrl)
import System.FilePath ((</>))

writeRequest :: forall m. (MonadIO m, MonadThrow m) => BaseUrl -> HttpClient.Request -> Int -> FilePath -> m ()
writeRequest baseUrl request counter testFolder = do
  body <- getBody $ HttpClient.requestBody request
  liftIO . LazyText.writeFile filepath . TextBuilder.toLazyText $
    requestLine
      <> requestHeaders
      <> "\n"
      <> body
  where
    filename = "agent-request-" <> show counter <> ".http"
    filepath = testFolder </> filename

    requestLine :: TextBuilder.Builder
    requestLine = method <> " " <> requestBaseUrl <> requestPath <> "\n"

    requestBaseUrl :: TextBuilder.Builder
    requestBaseUrl = TextBuilder.fromString $ showBaseUrl baseUrl

    requestPath :: TextBuilder.Builder
    requestPath = bsToBuilder $ HttpClient.path request

    method :: TextBuilder.Builder
    method = bsToBuilder $ HttpClient.method request

    requestHeaders :: TextBuilder.Builder
    requestHeaders =
      HttpClient.requestHeaders request
        & fmap formatHeader
        & mconcat

    formatHeader :: Header -> TextBuilder.Builder
    formatHeader (name, value) =
      let nameBuilder = bsToBuilder $ CI.original name
          valueBuilder =
            if name `Set.member` HttpClient.redactHeaders request
              then "{{" <> nameBuilder <> "}}" -- Make the header value a template variable
              else bsToBuilder value
       in nameBuilder <> ": " <> valueBuilder <> "\n"

    getBody :: HttpClient.RequestBody -> m TextBuilder.Builder
    getBody = \case
      HttpClient.RequestBodyLBS lazyBs -> pure $ formatIfJson (HttpClient.requestHeaders request) lazyBs
      HttpClient.RequestBodyBS bs -> pure . formatIfJson (HttpClient.requestHeaders request) $ LazyBS.fromStrict bs
      HttpClient.RequestBodyBuilder _ _ ->
        throwM $ UnsupportedError "Recording builder request bodies is not supported"
      HttpClient.RequestBodyStream _ _ ->
        throwM $ UnsupportedError "Recording stream request bodies is not supported"
      HttpClient.RequestBodyStreamChunked _ ->
        throwM $ UnsupportedError "Recording stream chunked request bodies is not supported"
      HttpClient.RequestBodyIO bodyIO ->
        liftIO bodyIO >>= getBody

writeResponse :: forall m. (MonadIO m) => HttpClient.Response LazyBS.ByteString -> Int -> FilePath -> m ()
writeResponse response counter testFolder =
  liftIO . LazyText.writeFile filepath . TextBuilder.toLazyText $
    statusLine
      <> responseHeaders
      <> "\n"
      <> body
  where
    filename = "agent-response-" <> show counter <> ".http"
    filepath = testFolder </> filename

    statusLine :: TextBuilder.Builder
    statusLine = httpVersion <> " " <> httpStatusCode <> " " <> httpStatusMessage <> "\n"

    httpVersion :: TextBuilder.Builder
    httpVersion = TextBuilder.fromString . show $ HttpClient.responseVersion response

    httpStatusCode :: TextBuilder.Builder
    httpStatusCode = TextBuilder.fromString . show . statusCode $ HttpClient.responseStatus response

    httpStatusMessage :: TextBuilder.Builder
    httpStatusMessage = bsToBuilder . statusMessage $ HttpClient.responseStatus response

    responseHeaders :: TextBuilder.Builder
    responseHeaders =
      HttpClient.responseHeaders response
        & fmap formatHeader
        & mconcat

    formatHeader :: Header -> TextBuilder.Builder
    formatHeader (name, value) =
      let nameBuilder = bsToBuilder $ CI.original name
          valueBuilder = bsToBuilder value
       in nameBuilder <> ": " <> valueBuilder <> "\n"

    body :: TextBuilder.Builder
    body = formatIfJson (HttpClient.responseHeaders response) $ HttpClient.responseBody response

isJsonBody :: [Header] -> Bool
isJsonBody headers =
  "application/json" `elem` contentTypeValueParts
  where
    contentTypeHeader = lookup (CI.mk "Content-Type") headers
    contentTypeValueParts = maybe [] (fmap Text.strip . Text.splitOn ";" . Text.decodeUtf8) contentTypeHeader

formatIfJson :: [Header] -> LazyBS.ByteString -> TextBuilder.Builder
formatIfJson headers body =
  if isJsonBody headers
    then do
      case Aeson.decode body of
        Just (json :: Aeson.Value) -> Aeson.encodePrettyToTextBuilder' (Aeson.defConfig {Aeson.confIndent = Aeson.Spaces 2}) json
        Nothing -> lbsToBuilder body
    else lbsToBuilder body

lbsToBuilder :: LazyBS.ByteString -> TextBuilder.Builder
lbsToBuilder = TextBuilder.fromLazyText . LazyText.decodeUtf8

bsToBuilder :: BS.ByteString -> TextBuilder.Builder
bsToBuilder = lbsToBuilder . LazyBS.fromStrict

data UnsupportedError = UnsupportedError String
  deriving stock (Show)
  deriving anyclass (Exception)
