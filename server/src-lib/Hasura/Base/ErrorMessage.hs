-- | Error messages
--
--   This module defines a type for user facing error messages.
--
--   To construct a value of this type, use `toErrorMessage` or the 'IsString' interface,
--   the type class 'ToErrorValue' defined in the "Hasura.Base.ToErrorValue" module,
--   or use the utility functions defined in the "Hasura.Base.ErrorValue" module.
--
--   'ErrorMessage's can also be composed using the 'Semigroup' interface.
--
--   To fail with an error message in monadic code, use the 'failWithMessage' function.
module Hasura.Base.ErrorMessage
  ( ErrorMessage,
    ErrorMessageOr,
    toErrorMessage,
    fromErrorMessage,
    failWithMessage,
  )
where

import Data.Aeson
import Data.String (IsString (..))
import Data.Text qualified as Text
import Hasura.Prelude

-- | 'ErrorMessage' wraps a 'Text' value such that it's easy to build up,
-- but difficult to break apart or extract the underlying text value.
newtype ErrorMessage = ErrorMessage
  { -- | A temporary extractor which will go away once 'ErrorMessage' is pervasive.
    fromErrorMessage :: Text
  }
  deriving newtype (Eq, Semigroup, ToJSON)

-- | A smart constructor for 'ErrorMessage' so that it cannot be deconstructed.
toErrorMessage :: Text -> ErrorMessage
toErrorMessage = ErrorMessage

-- | Fails with the given message.
failWithMessage :: MonadFail m => ErrorMessage -> m ()
failWithMessage = fail . Text.unpack . fromErrorMessage

instance IsString ErrorMessage where
  fromString = ErrorMessage . Text.pack

-- | A simple alias to prevent duplication.
type ErrorMessageOr = Either ErrorMessage
