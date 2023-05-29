{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}

module Hasura.RQL.Types.Webhook.Transform.Class
  ( Template (..),
    TemplatingEngine (..),
    TransformFn,
    TransformCtx,
    TransformErrorBundle (..),
    UnescapedTemplate (..),
  )
where

import Autodocodec (HasCodec (codec), dimapCodec, stringConstCodec)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Aeson qualified as J
import Data.Kind (Type)
import Hasura.Prelude

-- | Textual transformation template.
newtype Template = Template
  { unTemplate :: Text
  }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Hashable, FromJSONKey, ToJSONKey)
  deriving anyclass (NFData)

instance HasCodec Template where
  codec = dimapCodec Template unTemplate codec

instance J.FromJSON Template where
  parseJSON = J.withText "Template" (pure . Template)

instance J.ToJSON Template where
  toJSON = J.String . coerce

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
  deriving anyclass (NFData)

instance HasCodec UnescapedTemplate where
  codec = dimapCodec UnescapedTemplate getUnescapedTemplate codec

instance J.FromJSON UnescapedTemplate where
  parseJSON = J.withText "Template" (pure . UnescapedTemplate)

instance J.ToJSON UnescapedTemplate where
  toJSON = J.String . coerce

-------------------------------------------------------------------------------

-- | Available templating engines.
data TemplatingEngine
  = Kriti
  deriving stock (Bounded, Enum, Eq, Generic, Show)
  deriving anyclass (NFData)

instance HasCodec TemplatingEngine where
  codec = stringConstCodec [(Kriti, "Kriti")]

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

-- | The associated type 'TransformFn a' is the defunctionalized version
-- of some transformation that should be applied to a given request field.
--
-- In most cases it is some variation on a piece of template text describing
-- the transformation.
data family TransformFn a :: Type

data family TransformCtx a :: Type

-------------------------------------------------------------------------------

-- | We use collect all transformation failures as a '[J.Value]'.
newtype TransformErrorBundle = TransformErrorBundle
  { tebMessages :: [J.Value]
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (Monoid, Semigroup, FromJSON, ToJSON)
  deriving anyclass (NFData)
