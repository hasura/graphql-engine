{-# LANGUAGE NamedFieldPuns #-}

module System.Metrics.Prometheus.Validation
  ( isValidName,
    isValidHelpText,
    isValidLabelValue,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.Text as T

--------------------------------------------------------------------------------

-- | Test whether a string is a valid Prometheus name by checking that it
-- matches the regex @[a-zA-Z_][a-zA-Z0-9_]*@.
-- See
-- <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>
-- for more details.
isValidName :: T.Text -> Bool
isValidName name =
  case T.uncons name of
    Nothing -> False
    Just (headChar, _tail) ->
      isInitialNameChar headChar && T.all isNameChar name

isNameChar :: Char -> Bool
isNameChar c = isInitialNameChar c || isDigit c

isInitialNameChar :: Char -> Bool
isInitialNameChar c =
  isAsciiLower c || isAsciiUpper c || c == '_'

--------------------------------------------------------------------------------

-- | Test whether a string is valid Prometheus help text.
--
-- According to <https://prometheus.io/docs/instrumenting/exposition_formats/>,
-- help text may contain any sequence of UTF-8 characters, but the backslash
-- (@\\@) and line feed (@\\n@) characters have to be escaped as @\\\\@ and
-- @\\n@,
-- respectively.
isValidHelpText :: T.Text -> Bool
isValidHelpText =
  -- Using a strict left fold because we almost always expect a valid string,
  -- for which verification requires traversing the entire string.
  checkResult . T.foldl' f initialCheckState
  where
    f :: CheckState -> Char -> CheckState
    f CheckState {haveFoundError, isPrevCharBackslash} char =
      case char of
        '\n' -> CheckState True NotBackslash
        '\\' ->
          if isPrevCharBackslash == Backslash
            then CheckState haveFoundError NotBackslash
            else CheckState haveFoundError Backslash
        'n' -> CheckState haveFoundError NotBackslash
        _ ->
          CheckState
            (isPrevCharBackslash == Backslash || haveFoundError)
            NotBackslash

-- | Test whether a string is a valid Prometheus label value.
--
-- According to <https://prometheus.io/docs/instrumenting/exposition_formats/>,
-- label values can be any sequence of UTF-8 characters, but the backslash
-- (@\\@), double-quote (@"@), and line feed (@\\n@) characters have to be
-- escaped as @\\\\@, @\\"@, and @\\n@, respectively.
isValidLabelValue :: T.Text -> Bool
isValidLabelValue =
  -- Using a strict left fold because we almost always expect a valid string,
  -- for which verification requires traversing the entire string.
  checkResult . T.foldl' f initialCheckState
  where
    f :: CheckState -> Char -> CheckState
    f CheckState {haveFoundError, isPrevCharBackslash} char =
      case char of
        '\n' -> CheckState True NotBackslash
        '\\' ->
          if isPrevCharBackslash == Backslash
            then CheckState haveFoundError NotBackslash
            else CheckState haveFoundError Backslash
        'n' -> CheckState haveFoundError NotBackslash
        '\"' ->
          -- Extra case relative to `isValidHelpText`
          if isPrevCharBackslash == Backslash
            then CheckState haveFoundError NotBackslash
            else CheckState True NotBackslash
        _ ->
          CheckState
            (isPrevCharBackslash == Backslash || haveFoundError)
            NotBackslash

data CheckState = CheckState
  { haveFoundError :: !Bool,
    isPrevCharBackslash :: !IsBackslash
  }

data IsBackslash = Backslash | NotBackslash
  deriving (Eq)

initialCheckState :: CheckState
initialCheckState =
  CheckState
    { haveFoundError = False,
      isPrevCharBackslash = NotBackslash
    }

checkResult :: CheckState -> Bool
checkResult st =
  not (haveFoundError st) && isPrevCharBackslash st == NotBackslash
