module Hasura.Backends.DataConnector.IR.Function
  ( Name (..),
  )
where

import Data.Aeson (FromJSON, ToJSON, ToJSONKey (..))
import Data.Aeson.Types (toJSONKeyText)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Extended (ToTxt (..))
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue
import Hasura.Incremental (Cacheable)
import Hasura.Prelude

newtype Name = Name {unName :: NonEmpty Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype
    ( Cacheable,
      FromJSON,
      Hashable,
      NFData,
      ToJSON
    )

instance ToJSONKey Name where
  toJSONKey = toJSONKeyText toTxt

instance ToTxt Name where
  toTxt = Text.intercalate "." . NonEmpty.toList . unName

instance ToErrorValue Name where
  toErrorValue = ErrorValue.squote . toTxt
