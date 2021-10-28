module Hasura.RQL.DDL.RequestTransform
  ( applyRequestTransform,
    mkRequestTransform,
    RequestMethod (..),
    StringTemplateText (..),
    TemplatingEngine (..),
    TemplateText (..),
    ContentType (..),
    TransformHeaders (..),
    TransformErrorBundle (..),
    MetadataTransform (..),
    RequestTransform (..),
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
import Kriti.Parser (parserAndLexer)
import Network.HTTP.Client.Transformable qualified as HTTP
import Network.URI qualified as URI

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

-------------
--- Types ---
-------------

data TransformContext = TransformContext
  { tcUrl :: J.Value,
    tcBody :: J.Value,
    tcSessionVars :: J.Value,
    tcQueryParams :: J.Value
  }

-- | A set of data transformation functions and substitutions
-- generated from a MetadataTransform. Nothing values mean use the
-- original request value, unless otherwise indicated.
data RequestTransform = RequestTransform
  { -- | Change the request method to one provided here. Nothing means POST.
    rtRequestMethod :: Maybe RequestMethod,
    -- | A function which contructs a new URL given the request context.
    rtRequestURL :: Maybe (TransformContext -> Either TransformErrorBundle Text),
    -- | A function for transforming the request body.
    rtBody :: Maybe (TransformContext -> Either TransformErrorBundle J.Value),
    -- | Change content type to one provided here.
    rtContentType :: Maybe ContentType,
    -- | A function which contructs new query parameters given the request context.
    rtQueryParams :: Maybe (TransformContext -> Either TransformErrorBundle HTTP.Query),
    -- | A function which contructs new Headers given the request context.
    rtRequestHeaders :: Maybe (TransformContext -> Either TransformErrorBundle ([HTTP.Header] -> [HTTP.Header]))
  }

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
data MetadataTransform = MetadataTransform
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

instance NFData MetadataTransform

instance Cacheable MetadataTransform

infixr 8 .=?

(.=?) :: J.ToJSON v => k -> Maybe v -> Maybe (k, J.Value)
(.=?) _ Nothing = Nothing
(.=?) k (Just v) = Just (k, J.toJSON v)

instance J.ToJSON MetadataTransform where
  toJSON MetadataTransform {..} =
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

instance J.FromJSON MetadataTransform where
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
    pure $ MetadataTransform method url body contentType queryParams headers templateEngine'

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
    case parserAndLexer t of
      -- TODO: Use the parsed ValueExt in MetadataTransform so that we
      -- don't have to parse at every request.
      Right _ -> pure $ TemplateText t
      Left RenderedError {_message} -> fail $ T.unpack _message

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
     in case parserAndLexer wrapped of
          -- NOTE: We can't simplfy use the wrapped value because the
          -- 'FromJSONKey' instance uses coercion rather then this
          -- instance to wrap the newtype.  This means we would need to
          -- handle QueryParam keys explicitly in 'mkQueryParamsTransform'
          -- while values would be handled implicitliy via this aeson
          -- instance. That is confusing, so we just handle everything
          -- explicitly down the call stack.
          Right _ -> pure $ StringTemplateText t
          Left RenderedError {_message} -> fail $ T.unpack _message

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

---------------------------------------
--- Constructing Request Transforms ---
---------------------------------------

-- | Construct a `RequestTransform` from its metadata representation.
mkRequestTransform :: MetadataTransform -> RequestTransform
mkRequestTransform MetadataTransform {..} =
  let urlTransform = mkUrlTransform mtTemplatingEngine <$> mtRequestURL
      queryTransform = mkQueryParamsTransform mtTemplatingEngine <$> mtQueryParams
      headerTransform = mkHeaderTransform mtTemplatingEngine <$> mtRequestHeaders
      bodyTransform = mkTemplateTransform mtTemplatingEngine <$> mtBodyTransform
   in RequestTransform mtRequestMethod urlTransform bodyTransform mtContentType queryTransform headerTransform

mkQueryParamsTransform :: TemplatingEngine -> [(StringTemplateText, Maybe StringTemplateText)] -> TransformContext -> Either TransformErrorBundle HTTP.Query
mkQueryParamsTransform engine templates transformCtx =
  let transform t =
        case mkTemplateTransform engine (coerce wrapTemplate t) transformCtx of
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

-- | Given a `TransformHeaders` and the `TransformContext`, Construct a
-- function to transform the existing headers.
mkHeaderTransform :: TemplatingEngine -> TransformHeaders -> TransformContext -> Either TransformErrorBundle ([HTTP.Header] -> [HTTP.Header])
mkHeaderTransform engine TransformHeaders {..} transformCtx =
  let transform t =
        case mkTemplateTransform engine (coerce $ wrapTemplate t) transformCtx of
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

mkUrlTransform :: TemplatingEngine -> StringTemplateText -> TransformContext -> Either TransformErrorBundle Text
mkUrlTransform engine template transformCtx =
  case mkTemplateTransform engine (coerce $ wrapTemplate template) transformCtx of
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

-- | Construct a Template Transformation function
mkTemplateTransform :: TemplatingEngine -> TemplateText -> TransformContext -> Either TransformErrorBundle J.Value
mkTemplateTransform engine (TemplateText template) TransformContext {..} =
  let context = [("$base_url", tcUrl), ("$body", tcBody), ("$session_variables", tcSessionVars), ("$query_params", tcQueryParams)]
   in case engine of
        Kriti -> first (TransformErrorBundle . pure . J.toJSON) $ runKriti template context

buildTransformContext :: T.Text -> HTTP.Request -> Maybe SessionVariables -> TransformContext
buildTransformContext url reqData sessionVars =
  TransformContext
    { tcUrl = J.toJSON url,
      tcBody = fromMaybe J.Null $ J.decode @J.Value =<< view HTTP.body reqData,
      tcSessionVars = J.toJSON sessionVars,
      tcQueryParams = J.toJSON $ bimap TE.decodeUtf8 (fmap TE.decodeUtf8) <$> view HTTP.queryParams reqData
    }

-- | Transform an `HTTP.Request` with a `RequestTransform`.
--
-- Note: we pass in the request url explicitly for use in the
-- 'TransformContext'. We do this so that we can ensure that the url
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
applyRequestTransform :: T.Text -> RequestTransform -> HTTP.Request -> Maybe SessionVariables -> Either TransformErrorBundle HTTP.Request
applyRequestTransform reqUrl RequestTransform {..} reqData sessionVars =
  let transformCtx = buildTransformContext reqUrl reqData sessionVars
      method = fmap (TE.encodeUtf8 . renderRequestMethod) rtRequestMethod

      bodyFunc :: Maybe BL.ByteString -> Either TransformErrorBundle (Maybe BL.ByteString)
      bodyFunc body =
        case rtBody of
          Nothing -> pure body
          Just f -> pure . J.encode <$> f transformCtx

      urlFunc :: Text -> Either TransformErrorBundle Text
      urlFunc url =
        case rtRequestURL of
          Nothing -> pure url
          Just f -> f transformCtx

      queryFunc :: HTTP.Query -> Either TransformErrorBundle HTTP.Query
      queryFunc query =
        case rtQueryParams of
          Nothing -> pure query
          Just f -> f transformCtx

      headerFunc :: [HTTP.Header] -> Either TransformErrorBundle [HTTP.Header]
      headerFunc headers =
        case rtRequestHeaders of
          Nothing -> pure headers
          Just f -> f transformCtx >>= \g -> pure $ g headers

      contentTypeFunc :: [HTTP.Header] -> Either TransformErrorBundle [HTTP.Header]
      contentTypeFunc = pure . nubBy (\a b -> fst a == fst b) . (:) ("Content-Type", contentType)
        where
          contentType = maybe "application/json" (TE.encodeUtf8 . renderContentType) rtContentType
   in reqData & traverseOf HTTP.url urlFunc
        >>= traverseOf HTTP.body bodyFunc
        >>= traverseOf HTTP.queryParams queryFunc
        >>= traverseOf HTTP.method (pure . (`fromMaybe` method))
        >>= traverseOf HTTP.headers headerFunc
        >>= traverseOf HTTP.headers contentTypeFunc
