-- | Functions and datatypes for interpreting Postgres errors.
module Hasura.SQL.Error
  ( PGErrorType(..)
  , _PGDataException
  , _PGIntegrityConstraintViolation
  , _PGSyntaxErrorOrAccessRuleViolation
  , pgErrorType

  , PGErrorCode(..)
  , _PGErrorGeneric
  , _PGErrorSpecific

  , PGDataException(..)
  , PGIntegrityConstraintViolation(..)
  , PGSyntaxErrorOrAccessRuleViolation(..)
  ) where

import           Hasura.Prelude

import           Control.Lens.TH              (makePrisms)

import qualified Data.Text                    as T
import qualified Database.PG.Query.Connection as Q

-- | The top-level error code type. Errors in Postgres are divided into different /classes/, which
-- are further subdivided into individual error codes. Even if a particular status code is not known
-- to the application, it’s possible to determine its class and handle it appropriately.
data PGErrorType
  = PGDataException !(Maybe (PGErrorCode PGDataException))
  | PGIntegrityConstraintViolation !(Maybe (PGErrorCode PGIntegrityConstraintViolation))
  | PGSyntaxErrorOrAccessRuleViolation !(Maybe (PGErrorCode PGSyntaxErrorOrAccessRuleViolation))
  deriving (Show, Eq)

data PGErrorCode a
  = PGErrorGeneric
  -- ^ represents errors that have the non-specific @000@ status code
  | PGErrorSpecific !a
  -- ^ represents errors with a known, more specific status code
  deriving (Show, Eq, Functor)

data PGDataException
  = PGInvalidDatetimeFormat
  | PGInvalidParameterValue
  | PGInvalidEscapeSequence
  | PGInvalidTextRepresentation
  deriving (Show, Eq)

data PGIntegrityConstraintViolation
  = PGRestrictViolation
  | PGNotNullViolation
  | PGForeignKeyViolation
  | PGUniqueViolation
  | PGCheckViolation
  | PGExclusionViolation
  deriving (Show, Eq)

data PGSyntaxErrorOrAccessRuleViolation
  = PGUndefinedObject
  | PGInvalidColumnReference
  deriving (Show, Eq)

$(makePrisms ''PGErrorType)
$(makePrisms ''PGErrorCode)

pgErrorType :: Q.PGStmtErrDetail -> Maybe PGErrorType
pgErrorType errorDetails = parseTypes =<< Q.edStatusCode errorDetails
  where
    parseTypes fullCodeText = choice
      [ withClass "22" PGDataException
        [ code "007" PGInvalidDatetimeFormat
        , code "023" PGInvalidParameterValue
        , code "025" PGInvalidEscapeSequence
        , code "P02" PGInvalidTextRepresentation
        ]
      , withClass "23" PGIntegrityConstraintViolation
        [ code "001" PGRestrictViolation
        , code "502" PGNotNullViolation
        , code "503" PGForeignKeyViolation
        , code "505" PGUniqueViolation
        , code "514" PGCheckViolation
        , code "P01" PGExclusionViolation
        ]
      , withClass "42" PGSyntaxErrorOrAccessRuleViolation
        [ code "704" PGUndefinedObject
        , code "P10" PGInvalidColumnReference
        ]
      ]
      where
        (classText, codeText) = T.splitAt 2 fullCodeText

        withClass :: T.Text -> (Maybe a -> b) -> [Maybe a] -> Maybe b
        withClass expectedClassText mkClass codes =
          guard (classText == expectedClassText) $> mkClass (choice codes)

        code :: T.Text -> a -> Maybe (PGErrorCode a)
        code expectedCodeText codeValue =
          guard (codeText == expectedCodeText) $> PGErrorSpecific codeValue
