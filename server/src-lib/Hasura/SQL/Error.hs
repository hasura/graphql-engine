-- | Functions and datatypes for interpreting Postgres errors.
module Hasura.SQL.Error where

import           Hasura.Prelude

import           Control.Lens.TH              (makePrisms)

import qualified Data.Text                    as T
import qualified Database.PG.Query.Connection as Q

-- | The top-level error code type. Errors in Postgres are divided into different /classes/, which
-- are further subdivided into individual error codes. Even if a particular status code is not known
-- to the application, it’s possible to determine its class and handle it appropriately.
data PgErrorType
  = PgDataException !(Maybe (PgErrorCode PgDataException))
  | PgIntegrityConstraintViolation !(Maybe (PgErrorCode PgIntegrityConstraintViolation))
  | PgSyntaxErrorOrAccessRuleViolation !(Maybe (PgErrorCode PgSyntaxErrorOrAccessRuleViolation))
  deriving (Show, Eq)

data PgErrorCode a
  = PgErrorGeneric
  -- ^ represents errors that have the non-specific @000@ status code
  | PgErrorSpecific !a
  -- ^ represents errors with a known, more specific status code
  deriving (Show, Eq, Functor)

data PgDataException
  = PgInvalidDatetimeFormat
  | PgInvalidParameterValue
  | PgInvalidTextRepresentation
  deriving (Show, Eq)

data PgIntegrityConstraintViolation
  = PgRestrictViolation
  | PgNotNullViolation
  | PgForeignKeyViolation
  | PgUniqueViolation
  | PgCheckViolation
  | PgExclusionViolation
  deriving (Show, Eq)

data PgSyntaxErrorOrAccessRuleViolation
  = PgUndefinedObject
  | PgInvalidColumnReference
  deriving (Show, Eq)

$(makePrisms ''PgErrorType)
$(makePrisms ''PgErrorCode)

pgErrorType :: Q.PGStmtErrDetail -> Maybe PgErrorType
pgErrorType errorDetails = parseTypes =<< Q.edStatusCode errorDetails
  where
    parseTypes fullCodeText = choice
      [ withClass "22" PgDataException
        [ code "007" PgInvalidDatetimeFormat
        , code "023" PgInvalidParameterValue
        , code "P02" PgInvalidTextRepresentation
        ]
      , withClass "23" PgIntegrityConstraintViolation
        [ code "001" PgRestrictViolation
        , code "502" PgNotNullViolation
        , code "503" PgForeignKeyViolation
        , code "505" PgUniqueViolation
        , code "514" PgCheckViolation
        , code "P01" PgExclusionViolation
        ]
      , withClass "42" PgSyntaxErrorOrAccessRuleViolation
        [ code "704" PgUndefinedObject
        , code "P10" PgInvalidColumnReference
        ]
      ]
      where
        (classText, codeText) = T.splitAt 2 fullCodeText

        withClass :: T.Text -> (Maybe a -> b) -> [Maybe a] -> Maybe b
        withClass expectedClassText mkClass codes =
          guard (classText == expectedClassText) $> mkClass (choice codes)

        code :: T.Text -> a -> Maybe (PgErrorCode a)
        code expectedCodeText codeValue =
          guard (codeText == expectedCodeText) $> PgErrorSpecific codeValue

pgErrorToText :: Q.PGStmtErrDetail -> T.Text
pgErrorToText errorDetail =
  fromMaybe "postgres error" (Q.edMessage errorDetail)
    <> maybe "" formatDescription (Q.edDescription errorDetail)
    <> maybe "" formatHint (Q.edHint errorDetail)
  where
    formatDescription description = ";\n" <> prefixLines "  " description
    formatHint hint = "\n  hint: " <> prefixLinesExceptFirst "    " hint

    prefixLinesExceptFirst prefix content =
      T.intercalate ("\n" <> prefix) (T.lines content)
    prefixLines prefix content =
      prefix <> prefixLinesExceptFirst prefix content
