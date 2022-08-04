module Hasura.Backends.DataConnector.IR.Table
  ( Name (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON, ToJSONKey (..), withText)
import Data.Aeson.Types (toJSONKeyText)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Text.Extended (ToTxt (..))
import Hasura.Backends.DataConnector.API qualified as API
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Base.ToErrorValue
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Witch.From qualified as Witch

-- | The fully qualified name of a table. The last element in the list is the table name
-- and all other elements represent namespacing of the table name.
-- For example, for a database that has schemas, the name would be '[<schema>,<table name>]'
newtype Name = Name {unName :: NonEmpty Text}
  deriving stock (Data, Eq, Generic, Ord, Show)
  deriving newtype
    ( Cacheable,
      Hashable,
      NFData,
      ToJSON
    )

instance FromJSON Name where
  parseJSON value =
    Name <$> parseJSON value
      -- Fallback parsing of a single string to support older metadata
      <|> withText "Name" (\text -> pure . Name $ text :| []) value

instance ToJSONKey Name where
  toJSONKey = toJSONKeyText toTxt

instance Witch.From API.TableName Name where
  from (API.TableName n) = Name n

instance Witch.From Name API.TableName where
  from (Name n) = API.TableName n

instance ToTxt Name where
  toTxt = Text.intercalate "." . NonEmpty.toList . unName

instance ToErrorValue Name where
  toErrorValue = ErrorValue.squote . toTxt
