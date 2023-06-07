{-# LANGUAGE DeriveAnyClass #-}

module Test.HttpFile
  ( writeRequest,
    writeResponse,
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson qualified as J
import Data.Aeson.Encode.Pretty qualified as J
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
import Servant.Client (BaseUrl (baseUrlPath), showBaseUrl)
import System.FilePath ((</>))
import Prelude

writeRequest :: forall m. (MonadIO m, MonadThrow m) => BaseUrl -> HttpClient.Request -> String -> FilePath -> m ()
writeRequest baseUrl request fileNamePrefix testFolder = do
  body <- getBody $ HttpClient.requestBody request
  liftIO . LazyText.writeFile filepath . TextBuilder.toLazyText $
    requestLine
      <> requestHeaders
      <> "\n"
      <> body
  where
    filename = fileNamePrefix <> "-agent-request.http"
    filepath = testFolder </> filename

    requestLine :: TextBuilder.Builder
    requestLine = method <> " " <> requestBaseUrl <> requestPath <> "\n"

    requestBaseUrl :: TextBuilder.Builder
    requestBaseUrl = TextBuilder.fromString $ showBaseUrl (baseUrl {baseUrlPath = ""})

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
      HttpClient.RequestBodyLBS lazyBs -> pure $ transformAndFormatIfJson id (HttpClient.requestHeaders request) lazyBs
      HttpClient.RequestBodyBS bs -> pure . transformAndFormatIfJson id (HttpClient.requestHeaders request) $ LazyBS.fromStrict bs
      HttpClient.RequestBodyBuilder _ _ ->
        throwM $ UnsupportedError "Recording builder request bodies is not supported"
      HttpClient.RequestBodyStream _ _ ->
        throwM $ UnsupportedError "Recording stream request bodies is not supported"
      HttpClient.RequestBodyStreamChunked _ ->
        throwM $ UnsupportedError "Recording stream chunked request bodies is not supported"
      HttpClient.RequestBodyIO bodyIO ->
        liftIO bodyIO >>= getBody

writeResponse :: forall m. (MonadIO m) => (J.Value -> J.Value) -> HttpClient.Response LazyBS.ByteString -> String -> FilePath -> m ()
writeResponse redactJsonResponse response fileNamePrefix testFolder =
  liftIO . LazyText.writeFile filepath . TextBuilder.toLazyText $
    statusLine
      <> responseHeaders
      <> "\n"
      <> body
  where
    filename = fileNamePrefix <> "-agent-response.http"
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
    body = transformAndFormatIfJson redactJsonResponse (HttpClient.responseHeaders response) $ HttpClient.responseBody response

isJsonBody :: [Header] -> Bool
isJsonBody headers =
  "application/json" `elem` contentTypeValueParts
  where
    contentTypeHeader = lookup (CI.mk "Content-Type") headers
    contentTypeValueParts = maybe [] (fmap Text.strip . Text.splitOn ";" . Text.decodeUtf8) contentTypeHeader

transformAndFormatIfJson :: (J.Value -> J.Value) -> [Header] -> LazyBS.ByteString -> TextBuilder.Builder
transformAndFormatIfJson transform headers body =
  if isJsonBody headers
    then do
      case J.decode body of
        Just (json :: J.Value) -> J.encodePrettyToTextBuilder' (J.defConfig {J.confIndent = J.Spaces 2}) $ transform json
        Nothing -> lbsToBuilder body
    else lbsToBuilder body

lbsToBuilder :: LazyBS.ByteString -> TextBuilder.Builder
lbsToBuilder = TextBuilder.fromLazyText . LazyText.decodeUtf8

bsToBuilder :: BS.ByteString -> TextBuilder.Builder
bsToBuilder = lbsToBuilder . LazyBS.fromStrict

data UnsupportedError = UnsupportedError String
  deriving stock (Show)
  deriving anyclass (Exception)
