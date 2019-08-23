module Data.Aeson.Extended
       ( module J
       , encodeToStrictText
       ) where

import           Hasura.Prelude

import           Data.Aeson as J
import qualified Data.Aeson.Text               as JT
import qualified Data.Text.Lazy                as LT

encodeToStrictText :: (ToJSON a) => a -> Text
encodeToStrictText = LT.toStrict . JT.encodeToLazyText
