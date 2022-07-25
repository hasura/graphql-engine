-- | A type class and instances for 'ErrorMessage'.
--   See the "Hasura.Base.ErrorMessage" module for more information.
module Hasura.Base.ToErrorValue
  ( ToErrorValue (..),
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.Text qualified as Aeson
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text.Lazy qualified as Text.Lazy
import Hasura.Base.ErrorMessage
import Hasura.Base.ErrorValue
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

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
-- > [Aeson.Number 1, Aeson.Bool True, Aeson.String "three"]
--   Will be printed as:
-- > "[1, true, \"three\"]"
instance ToErrorValue a => ToErrorValue [a] where
  toErrorValue values = sconcat $ "[" :| List.intersperse (toErrorMessage ", ") (map toErrorValue values) ++ ["]"]

-- | Will be printed as a list
instance ToErrorValue a => ToErrorValue (NonEmpty a) where
  toErrorValue = toErrorValue . NonEmpty.toList

-- | Will be printed as a list
instance ToErrorValue a => ToErrorValue (HashSet a) where
  toErrorValue = toErrorValue . HashSet.toList

-- | Will be printed with single quotes surrounding it
instance ToErrorValue G.Name where
  toErrorValue = squote . G.unName

-- | Will be printed with single quotes surrounding it
instance ToErrorValue G.GType where
  toErrorValue = squote . G.showGT

instance ToErrorValue Aeson.Key where
  toErrorValue = dquote . Aeson.Key.toText

instance ToErrorValue Aeson.Value where
  toErrorValue = toErrorMessage . Text.Lazy.toStrict . Aeson.encodeToLazyText
