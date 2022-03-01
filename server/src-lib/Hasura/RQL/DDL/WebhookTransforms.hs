{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.DDL.WebhookTransforms
  ( applyRequestTransform,
    applyResponseTransform,
    buildReqTransformCtx,
    buildRespTransformCtx,
    mkRequestTransform,
    mkResponseTransform,
    RequestMethod (..),
    RemoveOrTransform (..),
    StringTemplateText (..),
    TemplatingEngine (..),
    TemplateText (..),
    ReqTransformCtx (..),
    TransformErrorBundle (..),
    TransformHeaders (..),
    MetadataRequestTransform (..),
    MetadataResponseTransform (..),
    RequestTransform (..),
    ResponseTransform (..),
    Version (..),
  )
where

import Control.Lens (traverseOf, view)
import Data.Aeson qualified as J
import Data.Bifunctor (bimap, first)
import Data.Bitraversable
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.CaseInsensitive qualified as CI
import Data.Either.Validation
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Validation qualified as V
import Hasura.Incremental (Cacheable)
import Hasura.Prelude hiding (first)
import Hasura.Session (SessionVariables, getSessionVariableValue, mkSessionVariable)
import Kriti (SerializedError (..), runKriti, runKritiWith)
import Kriti.Error (CustomFunctionError (..), serialize)
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
    tcQueryParams :: Maybe J.Value,
    tcFunctions :: M.HashMap T.Text (J.Value -> Either CustomFunctionError J.Value)
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
    reqTransformRequestMethod :: Maybe (ReqTransformCtx -> Either TransformErrorBundle RequestMethod),
    -- | A function which contructs a new URL given the request context.
    reqTransformRequestURL :: Maybe (ReqTransformCtx -> Either TransformErrorBundle Text),
    -- | A function for transforming the request body.
    reqTransformBody :: Maybe (RemoveOrTransform (ReqTransformCtx -> Either TransformErrorBundle J.Value)),
    -- | A function which contructs new query parameters given the request context.
    reqTransformQueryParams :: Maybe (ReqTransformCtx -> Either TransformErrorBundle HTTP.Query),
    -- | A function which contructs new Headers given the request context.
    reqTransformRequestHeaders :: Maybe (ReqTransformCtx -> Either TransformErrorBundle ([HTTP.Header] -> [HTTP.Header]))
  }

-- | A set of data transformation functions generated from a
-- 'MetadataResponseTransform'. 'Nothing' means use the original
-- response value.
newtype ResponseTransform = ResponseTransform {respTransformBody :: Maybe (RemoveOrTransform (RespTransformCtx -> Either TransformErrorBundle J.Value))}

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
  { -- | The Schema Version
    mtVersion :: Version,
    -- | Change the request method to one provided here. Nothing means POST.
    mtRequestMethod :: Maybe StringTemplateText,
    -- | Template script for transforming the URL.
    mtRequestURL :: Maybe StringTemplateText,
    -- | Template script for transforming the request body.
    mtBodyTransform :: Maybe (RemoveOrTransform TemplateText),
    -- | A list of template scripts for constructing new Query Params.
    mtQueryParams :: Maybe [(StringTemplateText, Maybe StringTemplateText)],
    -- | A list of template scripts for constructing headers.
    mtRequestHeaders :: Maybe TransformHeaders,
    -- | The template engine to use for transformations. Default: Kriti
    mtTemplatingEngine :: TemplatingEngine
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Cacheable)

instance J.ToJSON MetadataRequestTransform where
  toJSON MetadataRequestTransform {..} =
    let body = case mtVersion of
          V1 -> case mtBodyTransform of
            Just (Transform template) -> Just ("body", J.toJSON template)
            _ -> Nothing
          V2 -> "body" .=? mtBodyTransform
     in J.object $
          [ "template_engine" J..= mtTemplatingEngine,
            "version" J..= mtVersion
          ]
            <> catMaybes
              [ "method" .=? mtRequestMethod,
                "url" .=? mtRequestURL,
                "query_params" .=? fmap M.fromList mtQueryParams,
                "request_headers" .=? mtRequestHeaders,
                body
              ]

instance J.FromJSON MetadataRequestTransform where
  parseJSON = J.withObject "Object" $ \o -> do
    version <- o J..:? "version" J..!= V1
    method <- o J..:? "method"
    url <- o J..:? "url"
    body <- case version of
      V1 -> do
        template :: (Maybe TemplateText) <- o J..:? "body"
        pure $ fmap Transform template
      V2 -> o J..:? "body"
    queryParams' <- o J..:? "query_params"
    let queryParams = fmap M.toList queryParams'
    headers <- o J..:? "request_headers"
    templateEngine <- o J..:? "template_engine" J..!= Kriti
    pure $ MetadataRequestTransform version method url body queryParams headers templateEngine

data MetadataResponseTransform = MetadataResponseTransform
  { -- | The Schema Version
    mrtVersion :: Version,
    -- | Template script for transforming the response body.
    mrtBodyTransform :: Maybe (RemoveOrTransform TemplateText),
    -- | The template engine to use for transformations. Default: Kriti
    mrtTemplatingEngine :: TemplatingEngine
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Cacheable)

instance J.ToJSON MetadataResponseTransform where
  toJSON MetadataResponseTransform {..} =
    let body = case mrtVersion of
          V1 -> case mrtBodyTransform of
            Just (Transform template) -> Just ("body", J.toJSON template)
            _ -> Nothing
          V2 -> "body" .=? mrtBodyTransform
     in J.object $
          [ "template_engine" J..= mrtTemplatingEngine,
            "version" J..= mrtVersion
          ]
            <> catMaybes [body]

instance J.FromJSON MetadataResponseTransform where
  parseJSON = J.withObject "Object" $ \o -> do
    version <- o J..:? "version" J..!= V1
    body <- case version of
      V1 -> do
        template :: (Maybe TemplateText) <- o J..:? "body"
        pure $ fmap Transform template
      V2 -> o J..:? "body"
    templateEngine <- o J..:? "template_engine"
    let templateEngine' = fromMaybe Kriti templateEngine
    pure $ MetadataResponseTransform version body templateEngine'

data RemoveOrTransform a = Remove | Transform a
  deriving stock (Show, Eq, Functor, Generic)
  deriving anyclass (NFData, Cacheable)

instance J.ToJSON a => J.ToJSON (RemoveOrTransform a) where
  toJSON = \case
    Remove -> J.object ["action" J..= ("remove" :: T.Text)]
    Transform a ->
      J.object
        [ "action" J..= ("transform" :: T.Text),
          "template" J..= J.toJSON a
        ]

instance J.FromJSON a => J.FromJSON (RemoveOrTransform a) where
  parseJSON = J.withObject "RemoveOrTransform" $ \o -> do
    action :: T.Text <- o J..: "action"
    if action == "remove"
      then pure Remove
      else do
        "transform" :: T.Text <- o J..: "action"
        template <- o J..: "template"
        pure $ Transform template

newtype RequestMethod = RequestMethod (CI.CI T.Text)
  deriving stock (Generic)
  deriving newtype (Show, Eq)
  deriving anyclass (NFData, Cacheable)

instance J.ToJSON RequestMethod where
  toJSON = J.String . CI.original . coerce

instance J.FromJSON RequestMethod where
  parseJSON = J.withText "RequestMethod" (pure . coerce . CI.mk)

-- | Available Template Languages
data TemplatingEngine = Kriti
  deriving stock (Show, Eq, Enum, Bounded, Generic)
  deriving anyclass (NFData, Cacheable)

renderTemplatingEngine :: TemplatingEngine -> Text
renderTemplatingEngine _ = "Kriti"

instance J.FromJSON TemplatingEngine where
  parseJSON = J.withText "TemplatingEngine" \case
    "Kriti" -> pure Kriti
    _ -> fail "Invalid TemplatingEngine"

instance J.ToJSON TemplatingEngine where
  toJSON = J.String . renderTemplatingEngine

data Version = V1 | V2
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData, Cacheable, Hashable)

instance J.FromJSON Version where
  parseJSON v = do
    version :: Int <- J.parseJSON v
    case version of
      1 -> pure V1
      2 -> pure V2
      i -> fail $ "expected 1 or 3, encountered " ++ show i

instance J.ToJSON Version where
  toJSON = \case
    V1 -> J.toJSON @Int 1
    V2 -> J.toJSON @Int 2

-- | Unparsed Kriti templating code
newtype TemplateText = TemplateText {unTemplateText :: T.Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Hashable, J.ToJSONKey, J.FromJSONKey)
  deriving anyclass (NFData, Cacheable)

instance J.FromJSON TemplateText where
  parseJSON = J.withText "TemplateText" \t ->
    let bs = TE.encodeUtf8 t
     in case parser bs of
          -- TODO: Use the parsed ValueExt in MetadataRequestTransform so that we
          -- don't have to parse at every request.
          Right _ -> pure $ TemplateText t
          Left err ->
            let SerializedError {_message} = serialize err
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
            let SerializedError {_message} = serialize err
             in fail $ T.unpack _message

instance J.ToJSON StringTemplateText where
  toJSON = J.String . coerce

-- | This newtype exists solely to anchor a `FromJSON` instance and is
-- eliminated in the `TransformHeaders` `FromJSON` instance.
newtype HeaderKey = HeaderKey {unHeaderKey :: CI.CI Text}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Cacheable)

instance J.FromJSON HeaderKey where
  parseJSON = J.withText "HeaderKey" \txt -> case CI.mk txt of
    key -> pure $ HeaderKey key

data TransformHeaders = TransformHeaders
  { addHeaders :: [(CI.CI Text, StringTemplateText)],
    removeHeaders :: [CI.CI Text]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Cacheable)

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
  deriving anyclass (NFData, Cacheable)

-------------------------------
--- Constructing Transforms ---
-------------------------------

-- | Construct a `RequestTransform` from its metadata representation.
mkRequestTransform :: MetadataRequestTransform -> RequestTransform
mkRequestTransform MetadataRequestTransform {..} =
  let methodTransform = mkMethodTransform mtTemplatingEngine <$> mtRequestMethod
      urlTransform = mkUrlTransform mtTemplatingEngine <$> mtRequestURL
      queryTransform = mkQueryParamsTransform mtTemplatingEngine <$> mtQueryParams
      headerTransform = mkHeaderTransform mtTemplatingEngine <$> mtRequestHeaders
      bodyTransform = fmap (mkReqTemplateTransform mtTemplatingEngine) <$> mtBodyTransform
   in RequestTransform methodTransform urlTransform bodyTransform queryTransform headerTransform

mkResponseTransform :: MetadataResponseTransform -> ResponseTransform
mkResponseTransform MetadataResponseTransform {..} =
  let bodyTransform = fmap (mkRespTemplateTransform mrtTemplatingEngine) <$> mrtBodyTransform
   in ResponseTransform bodyTransform

-- | Transform a String Template
transformST :: TemplatingEngine -> ReqTransformCtx -> StringTemplateText -> Either TransformErrorBundle B.ByteString
transformST engine transformCtx t =
  valueToString =<< mkReqTemplateTransform engine (coerce wrapTemplate t) transformCtx

mkMethodTransform :: TemplatingEngine -> StringTemplateText -> ReqTransformCtx -> Either TransformErrorBundle RequestMethod
mkMethodTransform engine template transformCtx =
  fmap (coerce . CI.mk . TE.decodeUtf8) $ transformST engine transformCtx template

mkQueryParamsTransform :: TemplatingEngine -> [(StringTemplateText, Maybe StringTemplateText)] -> ReqTransformCtx -> Either TransformErrorBundle HTTP.Query
mkQueryParamsTransform engine templates transformCtx =
  let transform = eitherToValidation . transformST engine transformCtx

      transformationResults :: [(Validation TransformErrorBundle B.ByteString, Maybe (Validation TransformErrorBundle B.ByteString))]
      transformationResults = fmap (bimap transform (fmap transform)) templates
   in validationToEither $ traverse (bitraverse id sequenceA) transformationResults

-- | Given a `TransformHeaders` and the `ReqTransformCtx`, Construct a
-- function to transform the existing headers.
mkHeaderTransform :: TemplatingEngine -> TransformHeaders -> ReqTransformCtx -> Either TransformErrorBundle ([HTTP.Header] -> [HTTP.Header])
mkHeaderTransform engine TransformHeaders {..} ctx = do
  add <- V.toEither $ fmap mappend $ traverse (bitraverse (pure . CI.map TE.encodeUtf8) (V.fromEither . transformST engine ctx)) addHeaders
  let filter' xs = filter (flip notElem (fmap (CI.map TE.encodeUtf8) removeHeaders) . fst) xs
  pure $ add . filter'

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
        Kriti -> first (TransformErrorBundle . pure . J.toJSON . serialize) $ runKritiWith (template) context tcFunctions

-- | Construct a Template Transformation function for Responses
mkRespTemplateTransform :: TemplatingEngine -> TemplateText -> RespTransformCtx -> Either TransformErrorBundle J.Value
mkRespTemplateTransform engine (TemplateText template) RespTransformCtx {..} =
  let context = [("$body", rtcBody), ("$request", rtcReqCtx)]
   in case engine of
        Kriti -> first (TransformErrorBundle . pure . J.toJSON . serialize) $ runKriti template context

buildReqTransformCtx :: T.Text -> Maybe SessionVariables -> HTTP.Request -> ReqTransformCtx
buildReqTransformCtx url sessionVars reqData =
  ReqTransformCtx
    { tcUrl = Just $ J.toJSON url,
      tcBody = fromMaybe J.Null $ J.decode @J.Value =<< view HTTP.body reqData,
      tcSessionVars = J.toJSON sessionVars,
      tcQueryParams = Just $ J.toJSON $ bimap TE.decodeUtf8 (fmap TE.decodeUtf8) <$> view HTTP.queryParams reqData,
      tcFunctions = M.singleton "getSessionVariable" getSessionVar
    }
  where
    getSessionVar :: J.Value -> Either CustomFunctionError J.Value
    getSessionVar inp = case inp of
      J.String txt ->
        case sessionVarValue of
          Just x -> Right $ J.String x
          Nothing -> Left . CustomFunctionError $ "Session variable \"" <> txt <> "\" not found"
        where
          sessionVarValue = sessionVars >>= getSessionVariableValue (mkSessionVariable txt)
      _ -> Left $ CustomFunctionError "Session variable name should be a string"

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
      methodFunc :: B.ByteString -> Either TransformErrorBundle B.ByteString
      methodFunc method =
        case reqTransformRequestMethod of
          Nothing -> pure method
          Just f -> TE.encodeUtf8 . CI.original . coerce <$> f transformCtx

      bodyFunc :: Maybe BL.ByteString -> Either TransformErrorBundle (Maybe BL.ByteString)
      bodyFunc body =
        case reqTransformBody of
          Nothing -> pure body
          Just Remove -> pure Nothing
          Just (Transform f) -> pure . J.encode <$> f transformCtx

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
          Just f -> f transformCtx <*> pure headers
   in reqData & traverseOf HTTP.url urlFunc
        >>= traverseOf HTTP.body bodyFunc
        >>= traverseOf HTTP.queryParams queryFunc
        >>= traverseOf HTTP.method methodFunc
        >>= traverseOf HTTP.headers headerFunc

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
          Just Remove -> pure mempty
          Just (Transform f) -> J.encode <$> f ctx
   in bodyFunc (J.encode rtcBody)

-----------------
--- Utilities ---
-----------------

valueToString :: MonadError TransformErrorBundle m => J.Value -> m B.ByteString
valueToString = \case
  J.String str -> pure $ TE.encodeUtf8 str
  J.Number num -> pure $ TE.encodeUtf8 (tshow num)
  J.Bool True -> pure "true"
  J.Bool False -> pure "false"
  val -> throwErrorBundle "Template must produce a String, Number, or Boolean value" (Just val)

throwErrorBundle :: MonadError TransformErrorBundle m => T.Text -> Maybe J.Value -> m a
throwErrorBundle msg val =
  throwError $
    TransformErrorBundle $
      pure $
        J.object $
          [ "error_code" J..= J.String "TransformationError",
            "message" J..= J.String msg
          ]
            <> catMaybes [("value" J..=) <$> val]

infixr 8 .=?

(.=?) :: J.ToJSON v => k -> Maybe v -> Maybe (k, J.Value)
(.=?) _ Nothing = Nothing
(.=?) k (Just v) = Just (k, J.toJSON v)

-- | Wrap a template with escaped double quotes.
wrapTemplate :: StringTemplateText -> StringTemplateText
wrapTemplate (StringTemplateText t) = StringTemplateText $ "\"" <> t <> "\""
