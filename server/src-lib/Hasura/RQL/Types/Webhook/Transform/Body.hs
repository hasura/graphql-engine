{-# LANGUAGE DeriveAnyClass #-}

module Hasura.RQL.Types.Webhook.Transform.Body
  ( Body (..),
    BodyTransformFn (..),
    TransformFn (..),
    TransformCtx (..),
  )
where

import Autodocodec (HasCodec, codec, dimapCodec, disjointEitherCodec, object, requiredField', (.=))
import Autodocodec.Extended (discriminatorField)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Internal.Strict qualified as M
import Hasura.Prelude
import Hasura.RQL.Types.Webhook.Transform.Class (Template (..), TransformCtx, TransformFn, UnescapedTemplate (..))
import Hasura.RQL.Types.Webhook.Transform.Request (RequestTransformCtx (..))

-- | HTTP message body being transformed.
data Body
  = JSONBody (Maybe J.Value)
  | RawBody LBS.ByteString
  deriving stock (Eq, Show)

-- | The transformations which can be applied to an HTTP message body.
data BodyTransformFn
  = -- | Remove the HTTP message body.
    Remove
  | -- | Modify the JSON message body by applying a 'Template' transformation.
    ModifyAsJSON Template
  | -- | Modify the JSON message body by applying 'UnescapedTemplate'
    -- transformations to each field with a matching 'Text' key.
    ModifyAsFormURLEncoded (M.HashMap Text UnescapedTemplate)
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance FromJSON BodyTransformFn where
  parseJSON = J.withObject "BodyTransformFn" \o -> do
    action <- o J..: "action"
    case (action :: Text) of
      "remove" -> pure Remove
      "transform" -> do
        template <- o J..: "template"
        pure $ ModifyAsJSON template
      "x_www_form_urlencoded" -> do
        formTemplates <- o J..: "form_template"
        pure $ ModifyAsFormURLEncoded formTemplates
      _ -> fail "invalid transform action"

instance ToJSON BodyTransformFn where
  toJSON = \case
    Remove -> J.object ["action" J..= ("remove" :: Text)]
    ModifyAsJSON a ->
      J.object
        [ "action" J..= ("transform" :: Text),
          "template" J..= J.toJSON a
        ]
    ModifyAsFormURLEncoded formTemplates ->
      J.object
        [ "action" J..= ("x_www_form_urlencoded" :: Text),
          "form_template" J..= J.toJSON formTemplates
        ]

-- NOTE: GHC does not let us attach Haddock documentation to data family
-- instances, so 'BodyTransformFn' is defined separately from this wrapper.
newtype instance TransformFn Body = BodyTransformFn_ BodyTransformFn
  deriving stock (Eq, Generic, Show)
  deriving newtype (NFData, FromJSON, ToJSON)

newtype instance TransformCtx Body = TransformCtx RequestTransformCtx

instance HasCodec BodyTransformFn where
  codec =
    dimapCodec dec enc
      $ disjointEitherCodec removeCodec
      $ disjointEitherCodec modifyAsJSONCodec modifyAsFormURLEncodecCodec
    where
      removeCodec = object "BodyTransformFn_Remove" $ discriminatorField "action" "remove"

      modifyAsJSONCodec =
        dimapCodec snd ((),)
          $ object "BodyTransformFn_ModifyAsJSON"
          $ (,)
          <$> discriminatorField "action" "transform"
          .= fst
            <*> requiredField' @Template "template"
          .= snd

      modifyAsFormURLEncodecCodec =
        dimapCodec snd ((),)
          $ object "BodyTransformFn_ModifyAsFormURLEncoded"
          $ (,)
          <$> discriminatorField "action" "x_www_form_urlencoded"
          .= fst
            <*> requiredField' @(M.HashMap Text UnescapedTemplate) "form_template"
          .= snd

      dec (Left _) = Remove
      dec (Right (Left template)) = ModifyAsJSON template
      dec (Right (Right hashMap)) = ModifyAsFormURLEncoded hashMap

      enc Remove = Left ()
      enc (ModifyAsJSON template) = Right $ Left template
      enc (ModifyAsFormURLEncoded hashMap) = Right $ Right hashMap
