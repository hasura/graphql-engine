module Hasura.RQL.Types.ComputedField.Name
  ( ComputedFieldName (..),
    computedFieldNameToText,
    fromComputedField,
  )
where

import Autodocodec (HasCodec (codec), dimapCodec)
import Data.Aeson
import Data.Text.Extended (ToTxt)
import Data.Text.NonEmpty (NonEmptyText (unNonEmptyText))
import Database.PG.Query qualified as PG
import Hasura.Prelude
import Hasura.RQL.Types.Common (FieldName (..))

newtype ComputedFieldName = ComputedFieldName {unComputedFieldName :: NonEmptyText}
  deriving (Show, Eq, Ord, NFData, FromJSON, ToJSON, ToJSONKey, PG.ToPrepArg, ToTxt, Hashable, PG.FromCol, Generic)

instance HasCodec ComputedFieldName where
  codec = dimapCodec ComputedFieldName unComputedFieldName codec

computedFieldNameToText :: ComputedFieldName -> Text
computedFieldNameToText = unNonEmptyText . unComputedFieldName

fromComputedField :: ComputedFieldName -> FieldName
fromComputedField = FieldName . computedFieldNameToText
