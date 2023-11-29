-- | Functions and datatypes for interpreting MSSQL database errors.
module Hasura.Backends.MSSQL.SQL.Error
  ( ErrorClass (..),
    defaultMSSQLTxErrorHandler,
    mutationMSSQLTxErrorHandler,
    mkMSSQLTxErrorHandler,

    -- * Exposed for testing
    ErrorSubclass (..),
    DataExceptionSubclass (..),
    SyntaxErrorOrAccessViolationSubclass (..),
    parseErrorClass,
  )
where

import Data.Aeson
import Data.Text qualified as T
import Database.MSSQL.Transaction
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.Base.Error qualified as Error
import Hasura.Prelude

-- | The top-level error class. Errors in MSSQL are divided into different /classes/, which
-- are further subdivided into individual error subclasses. It is useful to determine the class of
-- database exception and handle it appropriately.
data ErrorClass
  = DataException (ErrorSubclass DataExceptionSubclass)
  | IntegrityConstraintViolation
  | SyntaxErrorOrAccessViolation (ErrorSubclass SyntaxErrorOrAccessViolationSubclass)
  deriving (Eq)

instance Show ErrorClass where
  show = \case
    DataException subclass -> withSubclass "Data exception." subclass
    IntegrityConstraintViolation -> "Integrity constraint violation."
    SyntaxErrorOrAccessViolation subclass -> withSubclass "Syntax error or access violation." subclass
    where
      withSubclass :: (Show a) => String -> ErrorSubclass a -> String
      withSubclass classError = \case
        NoSubclass -> classError
        Subclass subclass -> classError <> " " <> show subclass

data ErrorSubclass a
  = -- | represents non-specific @000@ subclass code
    NoSubclass
  | -- | represents known, more specific sub class
    Subclass a
  deriving (Eq)

data DataExceptionSubclass
  = StringDataRightTruncated
  | NumericValueOutOfRange
  | InvalidDatetimeFormat
  | DatetimeFieldOverflow
  | IntervalFieldOverflow
  | InvalidEscapeCharacter
  | InvalidEscapeSequence
  deriving (Eq)

instance Show DataExceptionSubclass where
  show = \case
    StringDataRightTruncated -> "String data, right-truncated."
    NumericValueOutOfRange -> "Numeric value out of range."
    InvalidDatetimeFormat -> "Invalid datetime format."
    DatetimeFieldOverflow -> "Datetime field overflow."
    IntervalFieldOverflow -> "Interval field overflow."
    InvalidEscapeCharacter -> "Invalid escape character."
    InvalidEscapeSequence -> "Invalid escape sequence."

data SyntaxErrorOrAccessViolationSubclass
  = TableOrViewAlreadyExists
  | TableOrViewNotFound
  | IndexAlreadyExists
  | IndexNotFound
  | ColumnAlreadyExists
  | ColumnNotFound
  deriving (Eq)

instance Show SyntaxErrorOrAccessViolationSubclass where
  show = \case
    TableOrViewAlreadyExists -> "Table or view already exists."
    TableOrViewNotFound -> "Table or view not found."
    IndexAlreadyExists -> "Index already exists."
    IndexNotFound -> "Index not found."
    ColumnAlreadyExists -> "Column already exists."
    ColumnNotFound -> "Column not found."

-- | Assign each error class' subclasses an appropriate API error code
errorClassCode :: ErrorClass -> Error.Code
errorClassCode = \case
  DataException _ -> Error.DataException
  IntegrityConstraintViolation -> Error.ConstraintViolation
  SyntaxErrorOrAccessViolation NoSubclass -> Error.BadRequest
  SyntaxErrorOrAccessViolation (Subclass subclass) -> case subclass of
    TableOrViewAlreadyExists -> Error.AlreadyExists
    TableOrViewNotFound -> Error.NotFound
    IndexAlreadyExists -> Error.AlreadyExists
    IndexNotFound -> Error.NotFound
    ColumnAlreadyExists -> Error.AlreadyExists
    ColumnNotFound -> Error.NotFound

-- | Parsing error class and subclass information from a SQLSTATE code.
-- SQLSTATE provides detailed information about the cause of a warning or error.
-- A SQLSTATE consists of 5 chars. They are divided into two parts: the first and
-- second chars contain a class and the following three a subclass.
parseErrorClass :: String -> Maybe ErrorClass
parseErrorClass sqlStateCode =
  choice
    [ withClass
        "22"
        DataException
        [ withSubclass "001" StringDataRightTruncated,
          withSubclass "003" NumericValueOutOfRange,
          withSubclass "007" InvalidDatetimeFormat,
          withSubclass "008" DatetimeFieldOverflow,
          withSubclass "015" IntervalFieldOverflow,
          withSubclass "019" InvalidEscapeCharacter,
          withSubclass "025" InvalidEscapeSequence
        ],
      withClass "23" (const IntegrityConstraintViolation) [],
      withClass
        "42"
        SyntaxErrorOrAccessViolation
        [ withSubclass "S01" TableOrViewAlreadyExists,
          withSubclass "S02" TableOrViewNotFound,
          withSubclass "S11" IndexAlreadyExists,
          withSubclass "S12" IndexNotFound,
          withSubclass "S21" ColumnAlreadyExists,
          withSubclass "S22" ColumnNotFound
        ]
    ]
  where
    (classText, subclassText) = T.splitAt 2 $ T.pack sqlStateCode

    withClass ::
      Text ->
      (ErrorSubclass a -> ErrorClass) ->
      [Maybe (ErrorSubclass a)] ->
      Maybe ErrorClass
    withClass expectedClassText mkClass subclasses = do
      guard (classText == expectedClassText)
      mkClass
        <$> if subclassText == "000"
          then Just NoSubclass
          else choice subclasses

    withSubclass :: Text -> a -> Maybe (ErrorSubclass a)
    withSubclass expectedSubclassText subclassValue =
      guard (subclassText == expectedSubclassText) $> Subclass subclassValue

-- | A default transaction error handler where all errors are unexpected.
defaultMSSQLTxErrorHandler :: MSSQLTxError -> Error.QErr
defaultMSSQLTxErrorHandler = mkMSSQLTxErrorHandler (const False)

-- | A transaction error handler to be used in constructing mutation transactions,
-- i.e INSERT, UPDATE and DELETE. We expect data exception and integrity constraint violation.
mutationMSSQLTxErrorHandler :: MSSQLTxError -> Error.QErr
mutationMSSQLTxErrorHandler = mkMSSQLTxErrorHandler $ \case
  DataException _ -> True
  IntegrityConstraintViolation -> True
  SyntaxErrorOrAccessViolation _ -> False

-- | Constructs a transaction error handler given a predicate that determines which error
-- classes (and subclasses) are expected and should be reported to the user. All other errors
-- are considered internal errors.
-- Example:-
--   Consider a insert mutation where we insert some data into columns of a table.
--   Except for the basic data type, such as Boolean, String, Float, Int etc.
--   we cannot invalidate data any further, such as validating timestamp string format.
--   In this case, a @'DataException' is expected from the database and it is handled and
--   thrown with proper error message.
mkMSSQLTxErrorHandler :: (ErrorClass -> Bool) -> MSSQLTxError -> Error.QErr
mkMSSQLTxErrorHandler isExpectedError = \case
  MSSQLQueryError query exception ->
    let unexpectedQueryError =
          (Error.internalError "database query error")
            { Error.qeInternal =
                Just
                  $ Error.ExtraInternal
                  $ object
                    [ "query" .= ODBC.renderQuery query,
                      "exception" .= odbcExceptionToJSONValue exception
                    ]
            }
     in fromMaybe unexpectedQueryError
          $ asExpectedError exception
          <&> \err -> err {Error.qeInternal = Just $ Error.ExtraInternal $ object ["query" .= ODBC.renderQuery query]}
  MSSQLConnError exception ->
    let unexpectedConnError =
          (Error.internalError "mssql connection error")
            { Error.qeInternal =
                Just
                  $ Error.ExtraInternal
                  $ object ["exception" .= odbcExceptionToJSONValue exception]
            }
     in fromMaybe unexpectedConnError $ asExpectedError exception
  MSSQLInternal err ->
    (Error.internalError "mssql internal error")
      { Error.qeInternal = Just $ Error.ExtraInternal $ object ["error" .= err]
      }
  where
    asExpectedError :: ODBC.ODBCException -> Maybe Error.QErr
    asExpectedError = \case
      ODBC.UnsuccessfulReturnCode _ _ databaseMessage sqlState -> do
        errorClass <- parseErrorClass =<< sqlState
        guard $ isExpectedError errorClass
        let errorMessage = show errorClass <> " " <> databaseMessage
        pure $ Error.err400 (errorClassCode errorClass) $ T.pack errorMessage
      _ -> Nothing

-- | The @'ODBC.ODBCException' type has no @'ToJSON' instance.
-- This is an attempt to convert the odbc exception to a JSON @'Value'
odbcExceptionToJSONValue :: ODBC.ODBCException -> Value
odbcExceptionToJSONValue exception =
  let (ty, message) = decodeODBCException exception
   in object ["type" .= ty, "message" .= message]
  where
    decodeODBCException :: ODBC.ODBCException -> (String, String)
    decodeODBCException = \case
      ODBC.UnsuccessfulReturnCode _ _ errMessage _ -> ("unsuccessful_return_code", errMessage)
      ODBC.AllocationReturnedNull _ -> ("allocation_returned_null", "allocating an ODBC resource failed")
      ODBC.UnknownDataType _ _ -> ("unknown_data_type", "An unsupported/unknown data type was returned from the ODBC driver")
      ODBC.DatabaseIsClosed _ -> ("database_is_closed", "Using database connection that is no longer available")
      ODBC.DatabaseAlreadyClosed -> ("database_already_closed", "The database is already closed")
      ODBC.NoTotalInformation _ -> ("no_total_information", "No total length information for column")
      ODBC.DataRetrievalError errMessage -> ("data_retrieval_error", errMessage)
