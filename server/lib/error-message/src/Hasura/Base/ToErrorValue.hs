-- | A type class and instances for 'ErrorMessage'.
--   See the "Hasura.Base.ErrorMessage" module for more information.
module Hasura.Base.ToErrorValue
  ( ToErrorValue (..),
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J.Key
import Data.Aeson.Text qualified as J
import Data.Foldable (fold)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text.Lazy qualified as Text.Lazy
import Data.Void (Void, absurd)
import Hasura.Base.ErrorMessage
import Hasura.Base.ErrorValue
import Language.GraphQL.Draft.Syntax qualified as G

-- | A type-specific mechanism for serializing a value to an error message fragment.
class ToErrorValue a where
  toErrorValue :: a -> ErrorMessage

instance ToErrorValue ErrorMessage where
  toErrorValue = id

instance ToErrorValue Void where
  toErrorValue = absurd

instance ToErrorValue () where
  toErrorValue = const "()"

-- | Wraps a list of values with brackets and separates the values with commas.
--   For example:
-- > [J.Number 1, J.Bool True, J.String "three"]
--   Will be printed as:
-- > "[1, true, \"three\"]"
instance (ToErrorValue a) => ToErrorValue [a] where
  toErrorValue values = "[" <> commaSeparatedValues <> "]"
    where
      commaSeparatedValues = fold $ List.intersperse (toErrorMessage ", ") (map toErrorValue values)

-- | Will be printed as a list
instance (ToErrorValue a) => ToErrorValue (NonEmpty a) where
  toErrorValue = toErrorValue . NonEmpty.toList

-- | Will be printed as a list
instance (ToErrorValue a) => ToErrorValue (HashSet a) where
  toErrorValue = toErrorValue . HashSet.toList

-- | Will be printed as a list
instance (ToErrorValue a) => ToErrorValue (Set a) where
  toErrorValue = toErrorValue . Set.toList

-- | Will be printed with single quotes surrounding it
instance ToErrorValue G.Name where
  toErrorValue = squote . G.unName

-- | Will be printed with single quotes surrounding it
instance ToErrorValue G.GType where
  toErrorValue = squote . G.showGT

instance ToErrorValue J.Key where
  toErrorValue = dquote . J.Key.toText

instance ToErrorValue J.Value where
  toErrorValue = toErrorMessage . Text.Lazy.toStrict . J.encodeToLazyText
