-- | Utility functions for building error messages.
--   See the "Hasura.Base.ErrorMessage" module for more information.
module Hasura.Base.ErrorValue
  ( bquote,
    squote,
    dquote,
    paren,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Hasura.Base.ErrorMessage

-- | Wrap error text in backticks
bquote :: Text -> ErrorMessage
bquote t = toErrorMessage $ Text.singleton '`' <> t <> Text.singleton '`'

-- | Wrap error text in single quotes
squote :: Text -> ErrorMessage
squote t = toErrorMessage $ Text.singleton '\'' <> t <> Text.singleton '\''

-- | Wrap error text in double quotes
dquote :: Text -> ErrorMessage
dquote t = toErrorMessage $ Text.singleton '"' <> t <> Text.singleton '"'

-- | Wrap error text in parenthesis
paren :: Text -> ErrorMessage
paren t = toErrorMessage $ "(" <> t <> ")"
