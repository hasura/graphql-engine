{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO(SOLOMON): Module Description.
module Hasura.RQL.DDL.Webhook.Transform.Class
  ( -- * Transformation Interface and Utilities
    Transform (..),

    -- ** Error Context
    TransformErrorBundle (..),
    throwErrorBundle,

    -- ** Request Transformation Context
    RequestTransformCtx (..),
    ResponseTransformCtx (..),
    mkReqTransformCtx,

    -- * Templating
    TemplatingEngine (..),
    Template (..),
    Version (..),
    mkRequestTemplateTransform,

    -- * Unescaped
    UnescapedTemplate (..),
    wrapUnescapedTemplate,
    mkUnescapedRequestTemplateTransform,
    validateUnescapedRequestTemplateTransform,
    mkUnescapedResponseTemplateTransform,
    validateUnescapedResponseTemplateTransform,
  )
where

-------------------------------------------------------------------------------

import Control.Arrow (left)
import Control.Lens (bimap, view)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson qualified as J
import Data.Binary.Builder (toLazyByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as M
import Data.Kind (Constraint, Type)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as TE
import Data.Validation (Validation, fromEither)
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.Session (SessionVariables, getSessionVariableValue, mkSessionVariable)
import Kriti (SerializedError (..), runKritiWith)
import Kriti.Error qualified as Kriti (CustomFunctionError (..), serialize)
import Kriti.Parser qualified as Kriti (parser)
import Network.HTTP.Client.Transformable qualified as HTTP

-------------------------------------------------------------------------------

-- | 'Transform' describes how to reify a defunctionalized transformation for
-- a particular request field.
type Transform :: Type -> Constraint
class Transform a where
  -- | The associated type 'TransformFn a' is the defunctionalized version
  -- of some transformation that should be applied to a given request field.
  --
  -- In most cases it is some variation on a piece of template text describing
  -- the transformation.
  data TransformFn a :: Type

  -- | 'transform' is a function which takes 'TransformFn' of @a@ and reifies
  -- it into a function of the form:
  --
  -- @
  --   ReqTransformCtx -> a -> m a
  -- @
  transform ::
    MonadError TransformErrorBundle m =>
    TransformFn a ->
    RequestTransformCtx ->
    a ->
    m a

-------------------------------------------------------------------------------

-- | We use collect all transformation failures as a '[J.Value]'.
newtype TransformErrorBundle = TransformErrorBundle
  { tebMessages :: [J.Value]
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup, FromJSON, ToJSON)
  deriving anyclass (Cacheable, NFData)

-- | A helper function for serializing transformation errors to JSON.
throwErrorBundle ::
  MonadError TransformErrorBundle m =>
  Text ->
  Maybe J.Value ->
  m a
throwErrorBundle msg val = do
  let requiredCtx =
        [ "error_code" J..= ("TransformationError" :: Text),
          "message" J..= msg
        ]
      optionalCtx =
        [ ("value" J..=) <$> val
        ]
      err = J.object (requiredCtx <> catMaybes optionalCtx)
  throwError $ TransformErrorBundle [err]

-------------------------------------------------------------------------------

-- | Common context that is made available to all request transformations.
data RequestTransformCtx = RequestTransformCtx
  { rtcBaseUrl :: Maybe J.Value,
    rtcBody :: J.Value,
    rtcSessionVariables :: J.Value,
    rtcQueryParams :: Maybe J.Value,
    rtcEngine :: TemplatingEngine,
    rtcFunctions :: M.HashMap T.Text (J.Value -> Either Kriti.CustomFunctionError J.Value)
  }

instance ToJSON RequestTransformCtx where
  toJSON RequestTransformCtx {..} =
    let required =
          [ "body" J..= rtcBody,
            "session_variables" J..= rtcSessionVariables
          ]
        optional =
          [ ("base_url" J..=) <$> rtcBaseUrl,
            ("query_params" J..=) <$> rtcQueryParams
          ]
     in J.object (required <> catMaybes optional)

-- | A smart constructor for constructing the 'RequestTransformCtx'
mkReqTransformCtx :: T.Text -> Maybe SessionVariables -> TemplatingEngine -> HTTP.Request -> RequestTransformCtx
mkReqTransformCtx url sessionVars engine reqData =
  RequestTransformCtx
    { rtcBaseUrl = Just $ J.toJSON url,
      rtcBody = fromMaybe J.Null $ J.decode @J.Value =<< view HTTP.body reqData,
      rtcSessionVariables = J.toJSON sessionVars,
      rtcQueryParams = Just $ J.toJSON $ bimap TE.decodeUtf8 (fmap TE.decodeUtf8) <$> view HTTP.queryParams reqData,
      rtcEngine = engine,
      rtcFunctions = M.singleton "getSessionVariable" getSessionVar
    }
  where
    getSessionVar :: J.Value -> Either Kriti.CustomFunctionError J.Value
    getSessionVar inp = case inp of
      J.String txt ->
        case sessionVarValue of
          Just x -> Right $ J.String x
          Nothing -> Left . Kriti.CustomFunctionError $ "Session variable \"" <> txt <> "\" not found"
        where
          sessionVarValue = sessionVars >>= getSessionVariableValue (mkSessionVariable txt)
      _ -> Left $ Kriti.CustomFunctionError "Session variable name should be a string"

-- | Common context that is made available to all response transformations.
data ResponseTransformCtx = ResponseTransformCtx
  { responseTransformBody :: J.Value,
    responseTransformReqCtx :: J.Value,
    responseTransformFunctions :: M.HashMap T.Text (J.Value -> Either Kriti.CustomFunctionError J.Value),
    responseTransformEngine :: TemplatingEngine
  }

-------------------------------------------------------------------------------

-- | Available templating engines.
data TemplatingEngine
  = Kriti
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (Cacheable, NFData)

-- XXX(jkachmar): We need roundtrip tests for these instances.
instance FromJSON TemplatingEngine where
  parseJSON =
    J.genericParseJSON
      J.defaultOptions
        { J.tagSingleConstructors = True
        }

-- XXX(jkachmar): We need roundtrip tests for these instances.
instance ToJSON TemplatingEngine where
  toJSON =
    J.genericToJSON
      J.defaultOptions
        { J.tagSingleConstructors = True
        }

  toEncoding =
    J.genericToEncoding
      J.defaultOptions
        { J.tagSingleConstructors = True
        }

-- | Validated textual transformation template.
newtype Template = Template
  { unTemplate :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Hashable, FromJSONKey, ToJSONKey)
  deriving anyclass (Cacheable, NFData)

-- XXX(jkachmar): We need roundtrip tests for these instances.
instance J.FromJSON Template where
  parseJSON = J.withText "Template" \t -> do
    case Kriti.parser (encodeUtf8 t) of
      -- TODO(SOLOMON): Use the parsed ValueExt in 'RequestFields' so that we
      -- don't have to parse at every request.
      Right _ -> pure $ Template t
      Left err ->
        let SerializedError {_message} = Kriti.serialize err
         in fail $ T.unpack _message

-- XXX(jkachmar): We need roundtrip tests for these instances.
instance J.ToJSON Template where
  toJSON = J.String . coerce

-- | A helper function for executing transformations from a 'Template'
-- and a 'RequestTrasformCtx'.
mkRequestTemplateTransform ::
  Template ->
  RequestTransformCtx ->
  Either TransformErrorBundle J.Value
mkRequestTemplateTransform template RequestTransformCtx {rtcEngine = Kriti, ..} =
  let context =
        [ ("$body", rtcBody),
          ("$session_variables", rtcSessionVariables)
        ]
          <> catMaybes
            [ ("$query_params",) <$> rtcQueryParams,
              ("$base_url",) <$> rtcBaseUrl
            ]
      eResult = runKritiWith (unTemplate $ template) context rtcFunctions
   in eResult & left \kritiErr ->
        let renderedErr = J.toJSON $ Kriti.serialize kritiErr
         in TransformErrorBundle [renderedErr]

mkResponseTemplateTransform ::
  Template ->
  ResponseTransformCtx ->
  Either TransformErrorBundle J.Value
mkResponseTemplateTransform template ResponseTransformCtx {responseTransformEngine = Kriti, ..} =
  let context = [("$body", responseTransformBody), ("$request", responseTransformReqCtx)]
      eResult = runKritiWith (unTemplate $ template) context responseTransformFunctions
   in eResult & left \kritiErr ->
        let renderedErr = J.toJSON $ Kriti.serialize kritiErr
         in TransformErrorBundle [renderedErr]

-------------------------------------------------------------------------------

-- | 'RequestTransform' Versioning
data Version
  = V1
  | V2
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Cacheable, Hashable, NFData)

instance J.FromJSON Version where
  parseJSON v = do
    version :: Int <- J.parseJSON v
    case version of
      1 -> pure V1
      2 -> pure V2
      i -> fail $ "expected 1 or 2, encountered " ++ show i

instance J.ToJSON Version where
  toJSON = \case
    V1 -> J.toJSON @Int 1
    V2 -> J.toJSON @Int 2

-------------------------------------------------------------------------------

-- | Validated textual transformation template /for string
-- interpolation only/.
--
-- This is necessary due to Kriti not distinguishing between string
-- literals and string templates.
newtype UnescapedTemplate = UnescapedTemplate
  { getUnescapedTemplate :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Hashable, FromJSONKey, ToJSONKey)
  deriving anyclass (Cacheable, NFData)

-- XXX(jkachmar): We need roundtrip tests for these instances.
instance J.FromJSON UnescapedTemplate where
  parseJSON = J.withText "UnescapedTemplate" \t -> do
    let wrapped = "\"" <> t <> "\""
    -- TODO(SOLOMON): Use the parsed ValueExt in 'RequestFields' so that we
    -- don't have to parse at every request.
    case Kriti.parser (encodeUtf8 wrapped) of
      -- NOTE: We can't simplfy use the wrapped value because the
      -- 'FromJSONKey' instance uses coercion rather then this
      -- instance to wrap the newtype.  This means we would need to
      -- handle QueryParam keys explicitly in 'mkQueryParamsTransform'
      -- while values would be handled implicitliy via this aeson
      -- instance. That is confusing, so we just handle everything
      -- explicitly down the call stack.
      Right _ -> pure $ UnescapedTemplate t
      Left err ->
        let SerializedError {_message} = Kriti.serialize err
         in fail $ T.unpack _message

-- XXX(jkachmar): We need roundtrip tests for these instances.
instance J.ToJSON UnescapedTemplate where
  toJSON = J.String . coerce

-- | Wrap an 'UnescapedTemplate' with escaped double quotes.
wrapUnescapedTemplate :: UnescapedTemplate -> Template
wrapUnescapedTemplate (UnescapedTemplate txt) = Template $ "\"" <> txt <> "\""

-- | A helper function for executing Kriti transformations from a
-- 'UnescapedTemplate' and a 'RequestTrasformCtx'. The differennce
-- from 'mkRequestTemplateTransform' is that this function will wrap
-- the template text in double quotes before running Kriti.
mkUnescapedRequestTemplateTransform ::
  RequestTransformCtx ->
  UnescapedTemplate ->
  Either TransformErrorBundle ByteString
mkUnescapedRequestTemplateTransform context unescapedTemplate = do
  result <-
    mkRequestTemplateTransform
      (wrapUnescapedTemplate unescapedTemplate)
      context
  encodeScalar result

-- | Run a Kriti transformation with an unescaped template in
-- 'Validation' instead of 'Either'.
validateUnescapedRequestTemplateTransform ::
  RequestTransformCtx ->
  UnescapedTemplate ->
  Validation TransformErrorBundle ByteString
validateUnescapedRequestTemplateTransform context unescapedTemplate =
  fromEither $
    mkUnescapedRequestTemplateTransform context unescapedTemplate

mkUnescapedResponseTemplateTransform ::
  ResponseTransformCtx ->
  UnescapedTemplate ->
  Either TransformErrorBundle ByteString
mkUnescapedResponseTemplateTransform context unescapedTemplate = do
  result <- mkResponseTemplateTransform (wrapUnescapedTemplate unescapedTemplate) context
  encodeScalar result

validateUnescapedResponseTemplateTransform ::
  ResponseTransformCtx ->
  UnescapedTemplate ->
  Validation TransformErrorBundle ByteString
validateUnescapedResponseTemplateTransform context unescapedTemplate =
  fromEither $
    mkUnescapedResponseTemplateTransform context unescapedTemplate

-------------------------------------------------------------------------------
-- Utility functions.

-- | Encode a JSON Scalar Value as a 'ByteString'.
-- If a non-Scalar value is provided, will return a 'TrnasformErrorBundle'
encodeScalar ::
  MonadError TransformErrorBundle m =>
  J.Value ->
  m ByteString
encodeScalar = \case
  J.String str -> pure $ encodeUtf8 str
  J.Number num ->
    pure . LBS.toStrict . toLazyByteString $ scientificBuilder num
  J.Bool True -> pure "true"
  J.Bool False -> pure "false"
  val ->
    throwErrorBundle "Template must produce a String, Number, or Boolean value" (Just val)
