-- | Error messages
--
-- This module defines a type for user facing error messages.
--
-- To construct a value of this type, use `toErrorMessage` or the 'IsString'
-- interface, the type class 'Hasura.Base.ToErrorValue' defined in the
-- "Hasura.Base.ToErrorValue" module, or use the utility functions defined in
-- the "Hasura.Base.ErrorValue" module.
--
-- 'ErrorMessage's can also be composed using the 'Semigroup' interface.
module Hasura.Base.ErrorMessage
  ( ErrorMessage,
    toErrorMessage,
    fromErrorMessage,
  )
where

import Data.Aeson
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text

-- | 'ErrorMessage' wraps a 'Text' value such that it's easy to build up,
-- but difficult to break apart or extract the underlying text value.
newtype ErrorMessage = ErrorMessage
  { -- | A temporary extractor which will go away once 'ErrorMessage' is pervasive.
    fromErrorMessage :: Text
  }
  deriving newtype (Eq, Semigroup, Monoid, ToJSON)

-- | A smart constructor for 'ErrorMessage' so that it cannot be deconstructed.
toErrorMessage :: Text -> ErrorMessage
toErrorMessage = ErrorMessage

instance IsString ErrorMessage where
  fromString = ErrorMessage . Text.pack
