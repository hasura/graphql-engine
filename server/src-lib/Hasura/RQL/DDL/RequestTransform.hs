module Hasura.RQL.DDL.RequestTransform
  ( applyRequestTransform,
    mkRequestTransform,
    mkRequestTransformDebug,
    RequestMethod (..),
    TemplatingEngine (..),
    TemplateText (..),
    ContentType (..),
    TransformHeaders (..),
    MetadataTransform (..),
    RequestTransform (..),
  )
where

import Control.Lens (over)
import Data.Aeson qualified as J
import Data.Bifunctor (bimap)
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as M
import Data.List (nubBy)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.Session (SessionVariables)
import Kriti (runKriti)
import Network.HTTP.Client.Transformable qualified as HTTP

{-

Request Transformations are data transformations used to modify HTTP
Requests before those requests are executed.

`MetadataTransform` values are stored in Metadata within the
`CreateAction` and `CreateEventTriggerQuery` Types and then converted
into `RequestTransform` values using `mkRequestTransform`.

`RequestTransforms` are applied to an HTTP Request using
`applyRequestTransform`.

In the case of body transformations, a user specified templating
script is applied. Currently a runtime failure of the template script
will return the original request body.

-}

-- | A set of data transformation functions and substitutions generated from a
-- MetadataTransform
data RequestTransform = RequestTransform
  { -- | Change the request method to one provided here. Nothing means POST.
    rtRequestMethod :: Maybe RequestMethod,
    -- | Change the request URL to one provided here. Nothing means original URL.
    rtRequestURL :: Maybe Text,
    -- | A function for transforming the request body using a Kriti template.
    rtBodyTransform :: J.Value -> Maybe SessionVariables -> J.Value,
    -- | Change content type to one provided here. Nothing means use original.
    rtContentType :: Maybe ContentType,
    -- | Change Request query params to those provided here. Nothing means use original.
    rtQueryParams :: Maybe HTTP.Query,
    -- | A transformation function for modifying the Request Headers.
    rtRequestHeaders :: [HTTP.Header] -> [HTTP.Header]
  }

data RequestMethod = GET | POST | PUT | PATCH | DELETE
  deriving (Show, Eq, Enum, Bounded, Generic)

renderRequestMethod :: RequestMethod -> Text
renderRequestMethod = \case
  GET -> "GET"
  POST -> "POST"
  PUT -> "PUT"
  PATCH -> "PATCH"
  DELETE -> "DELETE"

instance J.ToJSON RequestMethod where
  toJSON = J.String . renderRequestMethod

instance J.FromJSON RequestMethod where
  parseJSON = J.withText "RequestMethod" \case
    "GET" -> pure GET
    "POST" -> pure POST
    "PUT" -> pure PUT
    "PATCH" -> pure PATCH
    "DELETE" -> pure DELETE
    _ -> fail "Invalid Request Method"

instance NFData RequestMethod

instance Cacheable RequestMethod

-- | A de/serializable request transformation template which can be stored in
-- the metadata associated with an action/event trigger/etc. and used to produce
-- a 'RequestTransform'.
--
-- NOTE: This data type is _only_ intended to be parsed from and stored as user
-- metadata; no direct logical transformations should be done upon it.
--
-- NOTE: Users should convert this to 'RequestTransform' as close as possible to
-- the call site that performs a transformed HTTP request, as 'RequestTransform'
-- has a representation that makes it more difficult to deserialize for
-- debugging.
data MetadataTransform = MetadataTransform
  { -- | Change the request method to one provided here. Nothing means POST.
    mtRequestMethod :: Maybe RequestMethod,
    -- | Change the request URL to one provided here. Nothing means original URL.
    mtRequestURL :: Maybe Text,
    -- | Go-Basic template script for transforming the request body
    mtBodyTransform :: Maybe TemplateText,
    -- | Only the following Content-Types are allowed (default: application/json):
    mtContentType :: Maybe ContentType,
    -- | Replace any existing query params with those provided here
    mtQueryParams :: Maybe (M.HashMap Text (Maybe Text)),
    -- | Transform headers as defined here.
    mtRequestHeaders :: Maybe TransformHeaders,
    -- | The template engine to use for transformations. Default: Kriti
    mtTemplatingEngine :: TemplatingEngine
  }
  deriving (Show, Eq, Generic)

instance NFData MetadataTransform

instance Cacheable MetadataTransform

instance J.ToJSON MetadataTransform where
  toJSON MetadataTransform {..} =
    J.object
      [ "method" J..= mtRequestMethod,
        "url" J..= mtRequestURL,
        "body" J..= mtBodyTransform,
        "content_type" J..= mtContentType,
        "query_params" J..= mtQueryParams,
        "request_headers" J..= mtRequestHeaders,
        "template_engine" J..= mtTemplatingEngine
      ]

instance J.FromJSON MetadataTransform where
  parseJSON = J.withObject "Object" $ \o -> do
    method <- o J..:? "method"
    url <- o J..:? "url"
    body <- o J..:? "body"
    contentType <- o J..:? "content_type"
    queryParams <- o J..:? "query_params"
    headers <- o J..:? "request_headers"
    templateEngine <- o J..:? "template_engine"
    let templateEngine' = maybe Kriti id templateEngine
    pure $ MetadataTransform method url body contentType queryParams headers templateEngine'

-- | Available Template Languages
data TemplatingEngine = Kriti
  deriving (Show, Eq, Enum, Bounded, Generic)

renderTemplatingEngine :: TemplatingEngine -> Text
renderTemplatingEngine _ = "Kriti"

instance J.FromJSON TemplatingEngine where
  parseJSON = J.withText "TemplatingEngine" \case
    "Kriti" -> pure Kriti
    _ -> fail "Invalid TemplatingEngine"

instance J.ToJSON TemplatingEngine where
  toJSON = J.String . renderTemplatingEngine

instance NFData TemplatingEngine

instance Cacheable TemplatingEngine

newtype TemplateText = TemplateText T.Text
  deriving (Show, Eq, Generic)

instance NFData TemplateText

instance Cacheable TemplateText

instance J.FromJSON TemplateText where
  parseJSON = J.withText "TemplateText" (pure . TemplateText)

instance J.ToJSON TemplateText where
  toJSON = J.String . coerce

data ContentType = JSON | XWWWFORM
  deriving (Show, Eq, Enum, Bounded, Generic)

renderContentType :: ContentType -> Text
renderContentType = \case
  JSON -> "application/json"
  XWWWFORM -> "application/x-www-form-urlencoded"

instance J.ToJSON ContentType where
  toJSON = J.String . renderContentType

instance J.FromJSON ContentType where
  parseJSON = J.withText "ContentType" \case
    "application/json" -> pure JSON
    "application/x-www-form-urlencoded" -> pure XWWWFORM
    _ -> fail "Invalid ContentType"

instance NFData ContentType

instance Cacheable ContentType

-- | This newtype exists solely to anchor a `FromJSON` instance and is
-- eliminated in the `TransformHeaders` `FromJSON` instance.
newtype HeaderKey = HeaderKey {unHeaderKey :: CI.CI Text}
  deriving (Show, Eq, Ord, Generic)

instance NFData HeaderKey

instance Cacheable HeaderKey

instance J.FromJSON HeaderKey where
  parseJSON = J.withText "HeaderKey" \txt -> case CI.mk txt of
    "Content-Type" -> fail "Restricted Header: Content-Type"
    key -> pure $ HeaderKey key

data TransformHeaders = TransformHeaders
  { addHeaders :: [(CI.CI Text, Text)],
    removeHeaders :: [CI.CI Text]
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData TransformHeaders

instance Cacheable TransformHeaders

instance J.ToJSON TransformHeaders where
  toJSON TransformHeaders {..} =
    J.object
      [ "add_headers" J..= M.fromList (fmap (first CI.original) addHeaders),
        "remove_headers" J..= fmap CI.original removeHeaders
      ]

instance J.FromJSON TransformHeaders where
  parseJSON = J.withObject "TransformHeaders" $ \o -> do
    addHeaders :: M.HashMap Text Text <- fromMaybe mempty <$> o J..:? "add_headers"
    let headers = M.toList $ mapKeys CI.mk addHeaders
    removeHeaders <- o J..:? "remove_headers"
    let removeHeaders' = unHeaderKey <$> fromMaybe mempty removeHeaders
    pure $ TransformHeaders headers removeHeaders'

-- | Construct a `RequestTransform` from its metadata representation.
mkRequestTransform :: MetadataTransform -> RequestTransform
mkRequestTransform MetadataTransform {..} =
  let transformParams = fmap (bimap TE.encodeUtf8 (fmap TE.encodeUtf8)) . M.toList
      queryParams = transformParams <$> mtQueryParams
      headerTransform = maybe id mkHeaderTransform mtRequestHeaders
   in RequestTransform mtRequestMethod mtRequestURL (mkBodyTransform mtBodyTransform mtTemplatingEngine) mtContentType queryParams headerTransform

mkRequestTransformDebug :: MetadataTransform -> RequestTransform
mkRequestTransformDebug mt@MetadataTransform {mtBodyTransform, mtTemplatingEngine} =
  (mkRequestTransform mt) {rtBodyTransform = mkBodyTransform mtBodyTransform mtTemplatingEngine}

-- | Construct a Header Transformation function from a `TransformHeaders` value.
mkHeaderTransform :: TransformHeaders -> [HTTP.Header] -> [HTTP.Header]
mkHeaderTransform TransformHeaders {..} headers =
  let toBeRemoved = (fmap . CI.map) (TE.encodeUtf8) removeHeaders
      filteredHeaders = filter ((`notElem` toBeRemoved) . fst) headers
      newHeaders = fmap (bimap (CI.map TE.encodeUtf8) TE.encodeUtf8) addHeaders
   in newHeaders <> filteredHeaders

-- | Construct a Body Transformation function
mkBodyTransform :: Maybe TemplateText -> TemplatingEngine -> J.Value -> Maybe SessionVariables -> J.Value
mkBodyTransform Nothing _ source _ = source
mkBodyTransform (Just (TemplateText template)) engine source sessionVars =
  let context = [("$", source)] <> catMaybes [("$session",) . J.toJSON <$> sessionVars]
   in case engine of
        Kriti -> case runKriti template context of
          Left err -> J.toJSON err
          Right res -> res

-- | Transform an `HTTP.Request` with a `RequestTransform`.
applyRequestTransform :: RequestTransform -> HTTP.Request -> Maybe SessionVariables -> HTTP.Request
applyRequestTransform RequestTransform {..} reqData sessionVars =
  let method = fmap (TE.encodeUtf8 . renderRequestMethod) rtRequestMethod
      bodyFunc (Just b) = case J.decode @J.Value b of
        Just val -> pure $ J.encode $ rtBodyTransform val sessionVars
        Nothing -> pure b
      bodyFunc Nothing = Nothing
      contentType = maybe "application/json" (TE.encodeUtf8 . renderContentType) rtContentType
      headerFunc = nubBy (\a b -> fst a == fst b) . (:) ("Content-Type", contentType) . rtRequestHeaders
   in reqData & over HTTP.url (`fromMaybe` rtRequestURL)
        & over HTTP.method (`fromMaybe` method)
        & over HTTP.queryParams (`fromMaybe` rtQueryParams)
        & over HTTP.headers headerFunc
        & over HTTP.body bodyFunc
