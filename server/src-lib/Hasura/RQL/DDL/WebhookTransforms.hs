module Hasura.RQL.DDL.WebhookTransforms
  ( applyRequestTransform,
    applyResponseTransform,
    buildReqTransformCtx,
    buildRespTransformCtx,
    mkRequestTransform,
    mkResponseTransform,
    RequestMethod (..),
    StringTemplateText (..),
    TemplatingEngine (..),
    TemplateText (..),
    ContentType (..),
    ReqTransformCtx (..),
    TransformHeaders (..),
    TransformErrorBundle (..),
    MetadataRequestTransform (..),
    MetadataResponseTransform (..),
    RequestTransform (..),
    ResponseTransform (..),
  )
where

import Control.Lens (traverseOf, view)
import Data.Aeson qualified as J
import Data.Bifunctor (bimap, first)
import Data.Bitraversable
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Either.Validation
import Data.HashMap.Strict qualified as M
import Data.List (nubBy)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Hasura.Incremental (Cacheable)
import Hasura.Prelude hiding (first)
import Hasura.Session (SessionVariables)
import Kriti (RenderedError (..), runKriti)
import Kriti.Error (render)
import Kriti.Parser (parser)
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.URI qualified as URI

{-

Webhook Transformations are data transformations used to modify HTTP
Requests before those requests are executed or Responses after
requests are executed.

'MetadataRequestTransform'/'MetadataResponseTransform' values are
stored in Metadata within the 'CreateAction' and
'CreateEventTriggerQuery' Types and then converted into
'RequestTransform'/'ResponseTransform' values using
'mkRequestTransform'/'mkResponseTransform'.

'RequestTransforms' and 'ResponseTransforms' are applied to an HTTP
Request/Response using 'applyRequestTransform' and
'applyResponseTransform'.

In the case of body transformations, a user specified templating
script is applied.

-}

-------------
--- Types ---
-------------

data ReqTransformCtx = ReqTransformCtx
  { tcUrl :: Maybe J.Value,
    tcBody :: J.Value,
    tcSessionVars :: J.Value,
    tcQueryParams :: Maybe J.Value
  }

instance J.ToJSON ReqTransformCtx where
  toJSON ReqTransformCtx {..} =
    J.object $ ["base_url" J..= tcUrl, "body" J..= tcBody, "session_variables" J..= tcSessionVars] <> catMaybes [("query_params" J..=) <$> tcQueryParams]

data RespTransformCtx = RespTransformCtx
  { rtcBody :: J.Value,
    rtcReqCtx :: J.Value
  }

-- | A set of data transformation functions and substitutions
-- generated from a 'MetadataRequestTransform'. 'Nothing' values mean
-- use the original request value, unless otherwise indicated.
data RequestTransform = RequestTransform
  { -- | Change the request method to one provided here. Nothing means POST.
    reqTransformRequestMethod :: Maybe RequestMethod,
    -- | A function which contructs a new URL given the request context.
    reqTransformRequestURL :: Maybe (ReqTransformCtx -> Either TransformErrorBundle Text),
    -- | A function for transforming the request body.
    reqTransformBody :: Maybe (ReqTransformCtx -> Either TransformErrorBundle J.Value),
    -- | Change content type to one provided here.
    reqTransformContentType :: Maybe ContentType,
    -- | A function which contructs new query parameters given the request context.
    reqTransformQueryParams :: Maybe (ReqTransformCtx -> Either TransformErrorBundle HTTP.Query),
    -- | A function which contructs new Headers given the request context.
    reqTransformRequestHeaders :: Maybe (ReqTransformCtx -> Either TransformErrorBundle ([HTTP.Header] -> [HTTP.Header]))
  }

-- | A set of data transformation functions generated from a
-- 'MetadataResponseTransform'. 'Nothing' means use the original
-- response value.
newtype ResponseTransform = ResponseTransform {respTransformBody :: Maybe (RespTransformCtx -> Either TransformErrorBundle J.Value)}

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
--
-- Nothing values mean use the original request value, unless
-- otherwise indicated.
data MetadataRequestTransform = MetadataRequestTransform
  { -- | Change the request method to one provided here. Nothing means POST.
    mtRequestMethod :: Maybe RequestMethod,
    -- | Template script for transforming the URL.
    mtRequestURL :: Maybe StringTemplateText,
    -- | Template script for transforming the request body.
    mtBodyTransform :: Maybe TemplateText,
    -- | Replace the Content-Type with this value. Only
    -- "application/json" and "application/x-www-form-urlencoded" are
    -- allowed.
    mtContentType :: Maybe ContentType,
    -- | A list of template scripts for constructing new Query Params.
    mtQueryParams :: Maybe [(StringTemplateText, Maybe StringTemplateText)],
    -- | Transform headers as defined here.
    mtRequestHeaders :: Maybe TransformHeaders,
    -- | The template engine to use for transformations. Default: Kriti
    mtTemplatingEngine :: TemplatingEngine
  }
  deriving (Show, Eq, Generic)

instance NFData MetadataRequestTransform

instance Cacheable MetadataRequestTransform

infixr 8 .=?

(.=?) :: J.ToJSON v => k -> Maybe v -> Maybe (k, J.Value)
(.=?) _ Nothing = Nothing
(.=?) k (Just v) = Just (k, J.toJSON v)

instance J.ToJSON MetadataRequestTransform where
  toJSON MetadataRequestTransform {..} =
    J.object $
      ["template_engine" J..= mtTemplatingEngine]
        <> catMaybes
          [ "method" .=? mtRequestMethod,
            "url" .=? mtRequestURL,
            "body" .=? mtBodyTransform,
            "content_type" .=? mtContentType,
            "query_params" .=? fmap M.fromList mtQueryParams,
            "request_headers" .=? mtRequestHeaders
          ]

instance J.FromJSON MetadataRequestTransform where
  parseJSON = J.withObject "Object" $ \o -> do
    method <- o J..:? "method"
    url <- o J..:? "url"
    body <- o J..:? "body"
    contentType <- o J..:? "content_type"
    queryParams' <- o J..:? "query_params"
    let queryParams = fmap M.toList queryParams'
    headers <- o J..:? "request_headers"
    templateEngine <- o J..:? "template_engine"
    let templateEngine' = fromMaybe Kriti templateEngine
    pure $ MetadataRequestTransform method url body contentType queryParams headers templateEngine'

data MetadataResponseTransform = MetadataResponseTransform
  { mrtBodyTransform :: Maybe TemplateText,
    mrtTemplatingEngine :: TemplatingEngine
  }
  deriving (Show, Eq, Generic)

instance NFData MetadataResponseTransform

instance Cacheable MetadataResponseTransform

instance J.ToJSON MetadataResponseTransform where
  toJSON MetadataResponseTransform {..} =
    J.object $
      ["template_engine" J..= mrtTemplatingEngine]
        <> catMaybes ["body" .=? mrtBodyTransform]

instance J.FromJSON MetadataResponseTransform where
  parseJSON = J.withObject "Object" $ \o -> do
    body <- o J..:? "body"
    templateEngine <- o J..:? "template_engine"
    let templateEngine' = fromMaybe Kriti templateEngine
    pure $ MetadataResponseTransform body templateEngine'

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

-- | Unparsed Kriti templating code
newtype TemplateText = TemplateText {unTemplateText :: T.Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Hashable, J.ToJSONKey, J.FromJSONKey)

instance NFData TemplateText

instance Cacheable TemplateText

instance J.FromJSON TemplateText where
  parseJSON = J.withText "TemplateText" \t ->
    let bs = TE.encodeUtf8 t
     in case parser bs of
          -- TODO: Use the parsed ValueExt in MetadataRequestTransform so that we
          -- don't have to parse at every request.
          Right _ -> pure $ TemplateText t
          Left err ->
            let RenderedError {_message} = render err
             in fail $ T.unpack _message

instance J.ToJSON TemplateText where
  toJSON = J.String . coerce

-- | Unparsed Kriti templating code for string interpolations only.
-- NOTE: WE are highly coupled to Kriti at this point and will likely
-- need to rethink Kriti string interpolation when we start adding
-- other template engines.
newtype StringTemplateText = StringTemplateText {unStringTemplateText :: T.Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Hashable, J.ToJSONKey, J.FromJSONKey)

instance NFData StringTemplateText

instance Cacheable StringTemplateText

instance J.FromJSON StringTemplateText where
  parseJSON = J.withText "StringTemplateText" \t ->
    let wrapped = "\"" <> t <> "\""
     in case parser (TE.encodeUtf8 wrapped) of
          -- NOTE: We can't simplfy use the wrapped value because the
          -- 'FromJSONKey' instance uses coercion rather then this
          -- instance to wrap the newtype.  This means we would need to
          -- handle QueryParam keys explicitly in 'mkQueryParamsTransform'
          -- while values would be handled implicitliy via this aeson
          -- instance. That is confusing, so we just handle everything
          -- explicitly down the call stack.
          Right _ -> pure $ StringTemplateText t
          Left err ->
            let RenderedError {_message} = render err
             in fail $ T.unpack _message

instance J.ToJSON StringTemplateText where
  toJSON = J.String . coerce

-- | Wrap a template with escaped double quotes.
wrapTemplate :: StringTemplateText -> StringTemplateText
wrapTemplate (StringTemplateText t) = StringTemplateText $ "\"" <> t <> "\""

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
  { addHeaders :: [(CI.CI Text, StringTemplateText)],
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
    addHeaders <- fromMaybe mempty <$> o J..:? "add_headers"
    let headers = M.toList $ mapKeys CI.mk addHeaders
    removeHeaders <- o J..:? "remove_headers"
    let removeHeaders' = unHeaderKey <$> fromMaybe mempty removeHeaders
    pure $ TransformHeaders headers removeHeaders'

newtype TransformErrorBundle = TransformErrorBundle {teMessages :: [J.Value]}
  deriving stock (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid, J.ToJSON)

instance NFData TransformErrorBundle

instance Cacheable TransformErrorBundle

-------------------------------
--- Constructing Transforms ---
-------------------------------

-- | Construct a `RequestTransform` from its metadata representation.
mkRequestTransform :: MetadataRequestTransform -> RequestTransform
mkRequestTransform MetadataRequestTransform {..} =
  let urlTransform = mkUrlTransform mtTemplatingEngine <$> mtRequestURL
      queryTransform = mkQueryParamsTransform mtTemplatingEngine <$> mtQueryParams
      headerTransform = mkHeaderTransform mtTemplatingEngine <$> mtRequestHeaders
      bodyTransform = mkReqTemplateTransform mtTemplatingEngine <$> mtBodyTransform
   in RequestTransform mtRequestMethod urlTransform bodyTransform mtContentType queryTransform headerTransform

mkResponseTransform :: MetadataResponseTransform -> ResponseTransform
mkResponseTransform MetadataResponseTransform {..} =
  let bodyTransform = mkRespTemplateTransform mrtTemplatingEngine <$> mrtBodyTransform
   in ResponseTransform bodyTransform

mkQueryParamsTransform :: TemplatingEngine -> [(StringTemplateText, Maybe StringTemplateText)] -> ReqTransformCtx -> Either TransformErrorBundle HTTP.Query
mkQueryParamsTransform engine templates transformCtx =
  let transform t =
        case mkReqTemplateTransform engine (coerce wrapTemplate t) transformCtx of
          Left err -> Left err
          Right (J.String str) -> Right $ str
          Right (J.Number num) -> Right $ tshow num
          Right (J.Bool True) -> Right $ "true"
          Right (J.Bool False) -> Right $ "false"
          Right val ->
            Left $
              TransformErrorBundle $
                pure $
                  J.object
                    [ "error_code" J..= J.String "TransformationError",
                      "message" J..= J.String "Query Param Transforms must produce a String, Number, or Boolean value",
                      "value" J..= val
                    ]
      toValidation :: [(Either TransformErrorBundle T.Text, Maybe (Either TransformErrorBundle T.Text))] -> [(Validation TransformErrorBundle T.Text, Maybe (Validation TransformErrorBundle T.Text))]
      toValidation = fmap (bimap eitherToValidation (fmap eitherToValidation))

      collectErrors :: [(Either TransformErrorBundle T.Text, Maybe (Either TransformErrorBundle T.Text))] -> Validation TransformErrorBundle [(T.Text, Maybe T.Text)]
      collectErrors xs = traverse (bitraverse id sequenceA) (toValidation xs)

      results = fmap (bimap transform (fmap transform)) templates
      collectedResults = validationToEither $ collectErrors results
   in (fmap . fmap) (bimap TE.encodeUtf8 (fmap TE.encodeUtf8)) collectedResults

-- | Given a `TransformHeaders` and the `ReqTransformCtx`, Construct a
-- function to transform the existing headers.
mkHeaderTransform :: TemplatingEngine -> TransformHeaders -> ReqTransformCtx -> Either TransformErrorBundle ([HTTP.Header] -> [HTTP.Header])
mkHeaderTransform engine TransformHeaders {..} transformCtx =
  let transform t =
        case mkReqTemplateTransform engine (coerce $ wrapTemplate t) transformCtx of
          Left err -> Failure err
          Right (J.String str) -> Success $ TE.encodeUtf8 str
          Right (J.Number num) -> Success $ TE.encodeUtf8 $ tshow num
          Right (J.Bool True) -> Success "true"
          Right (J.Bool False) -> Success "false"
          Right val ->
            Failure $
              TransformErrorBundle $
                pure $
                  J.object
                    [ "error_code" J..= J.String "TransformationError",
                      "message" J..= J.String ("Header Transforms must produce a String, Number, or Boolean value: " <> tshow val)
                    ]

      failIfCT key =
        if CI.foldedCase key == "content-type"
          then
            Failure $
              TransformErrorBundle $
                pure $
                  J.object
                    [ "error_code" J..= J.String "TransformationError",
                      "message" J..= J.String ("Header Transforms cannot add Content-Type" <> CI.original key)
                    ]
          else Success $ CI.map TE.encodeUtf8 key

      toBeAdded = traverse (bitraverse failIfCT transform) addHeaders
      toBeRemoved = (fmap . CI.map) TE.encodeUtf8 removeHeaders

      filterHeaders h = filter ((`notElem` toBeRemoved) . fst) h
   in case toBeAdded of
        Failure err -> throwError err
        Success toBeAdded' -> pure (filterHeaders . (toBeAdded' <>))

mkUrlTransform :: TemplatingEngine -> StringTemplateText -> ReqTransformCtx -> Either TransformErrorBundle Text
mkUrlTransform engine template transformCtx =
  case mkReqTemplateTransform engine (coerce $ wrapTemplate template) transformCtx of
    Left err -> Left err
    Right (J.String url) ->
      case URI.parseURI (T.unpack url) of
        Just _ -> Right url
        Nothing ->
          Left $
            TransformErrorBundle $
              pure $
                J.object
                  [ "error_code" J..= J.String "TransformationError",
                    -- TODO: This error message is not very
                    -- helpful. We should find a way to identity what
                    -- is wrong with the URL.
                    "message" J..= J.String ("Invalid URL: " <> url)
                  ]
    Right val ->
      Left $
        TransformErrorBundle $
          pure $
            J.object
              [ "error_code" J..= J.String "TransformationError",
                "message" J..= J.String ("Url Transforms must produce a String value: " <> tshow val)
              ]

-- | Construct a Template Transformation function for Requests
mkReqTemplateTransform :: TemplatingEngine -> TemplateText -> ReqTransformCtx -> Either TransformErrorBundle J.Value
mkReqTemplateTransform engine (TemplateText template) ReqTransformCtx {..} =
  let context = [("$body", tcBody), ("$session_variables", tcSessionVars)] <> catMaybes [("$query_params",) <$> tcQueryParams, ("$base_url",) <$> tcUrl]
   in case engine of
        Kriti -> first (TransformErrorBundle . pure . J.toJSON) $ runKriti (TE.encodeUtf8 template) context

-- | Construct a Template Transformation function for Responses
mkRespTemplateTransform :: TemplatingEngine -> TemplateText -> RespTransformCtx -> Either TransformErrorBundle J.Value
mkRespTemplateTransform engine (TemplateText template) RespTransformCtx {..} =
  let context = [("$body", rtcBody), ("$request", rtcReqCtx)]
   in case engine of
        Kriti -> first (TransformErrorBundle . pure . J.toJSON) $ runKriti (TE.encodeUtf8 template) context

buildReqTransformCtx :: T.Text -> Maybe SessionVariables -> HTTP.Request -> ReqTransformCtx
buildReqTransformCtx url sessionVars reqData =
  ReqTransformCtx
    { tcUrl = Just $ J.toJSON url,
      tcBody = fromMaybe J.Null $ J.decode @J.Value =<< view HTTP.body reqData,
      tcSessionVars = J.toJSON sessionVars,
      tcQueryParams = Just $ J.toJSON $ bimap TE.decodeUtf8 (fmap TE.decodeUtf8) <$> view HTTP.queryParams reqData
    }

buildRespTransformCtx :: ReqTransformCtx -> BL.ByteString -> RespTransformCtx
buildRespTransformCtx reqCtx respBody =
  RespTransformCtx
    { rtcBody = fromMaybe J.Null $ J.decode @J.Value respBody,
      rtcReqCtx = J.toJSON reqCtx
    }

-- | Transform an `HTTP.Request` with a `RequestTransform`.
--
-- Note: we pass in the request url explicitly for use in the
-- 'ReqTransformCtx'. We do this so that we can ensure that the url
-- is syntactically identical to what the use submits. If we use the
-- parsed request from the 'HTTP.Request' term then it is possible
-- that the url is semantically equivalent but syntactically
-- different. An example of this is the presence or lack of a trailing
-- slash on the URL path. This important when performing string
-- interpolation on the request url.
--
-- Because of this requirement, we must ensure the sequence of
-- transformation applications. If a query param transformation
-- occured before the url transformation then the query param
-- transformation would be wiped out.
-- applyRequestTransform :: T.Text -> RequestTransform -> HTTP.Request -> Maybe SessionVariables -> Either TransformErrorBundle HTTP.Request
-- applyRequestTransform reqUrl RequestTransform {..} reqData sessionVars =
applyRequestTransform :: (HTTP.Request -> ReqTransformCtx) -> RequestTransform -> HTTP.Request -> Either TransformErrorBundle HTTP.Request
applyRequestTransform transformCtx' RequestTransform {..} reqData =
  let transformCtx = transformCtx' reqData
      method = fmap (TE.encodeUtf8 . renderRequestMethod) reqTransformRequestMethod

      bodyFunc :: Maybe BL.ByteString -> Either TransformErrorBundle (Maybe BL.ByteString)
      bodyFunc body =
        case reqTransformBody of
          Nothing -> pure body
          Just f -> pure . J.encode <$> f transformCtx

      urlFunc :: Text -> Either TransformErrorBundle Text
      urlFunc url =
        case reqTransformRequestURL of
          Nothing -> pure url
          Just f -> f transformCtx

      queryFunc :: HTTP.Query -> Either TransformErrorBundle HTTP.Query
      queryFunc query =
        case reqTransformQueryParams of
          Nothing -> pure query
          Just f -> f transformCtx

      headerFunc :: [HTTP.Header] -> Either TransformErrorBundle [HTTP.Header]
      headerFunc headers =
        case reqTransformRequestHeaders of
          Nothing -> pure headers
          Just f -> f transformCtx >>= \g -> pure $ g headers

      contentTypeFunc :: [HTTP.Header] -> Either TransformErrorBundle [HTTP.Header]
      contentTypeFunc = pure . nubBy (\a b -> fst a == fst b) . (:) ("Content-Type", contentType)
        where
          contentType = maybe "application/json" (TE.encodeUtf8 . renderContentType) reqTransformContentType
   in reqData & traverseOf HTTP.url urlFunc
        >>= traverseOf HTTP.body bodyFunc
        >>= traverseOf HTTP.queryParams queryFunc
        >>= traverseOf HTTP.method (pure . (`fromMaybe` method))
        >>= traverseOf HTTP.headers headerFunc
        >>= traverseOf HTTP.headers contentTypeFunc

-- | At the moment we only transform the body of
-- Responses. 'http-client' does not export the constructors for
-- 'Response'. If we want to transform then we will need additional
-- 'apply' functions.
applyResponseTransform :: ResponseTransform -> RespTransformCtx -> Either TransformErrorBundle BL.ByteString
applyResponseTransform ResponseTransform {..} ctx@RespTransformCtx {..} =
  let bodyFunc :: BL.ByteString -> Either TransformErrorBundle BL.ByteString
      bodyFunc body =
        case respTransformBody of
          Nothing -> pure body
          Just f -> J.encode <$> f ctx
   in bodyFunc (J.encode rtcBody)
