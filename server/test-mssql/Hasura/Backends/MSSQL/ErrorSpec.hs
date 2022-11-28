module Hasura.Backends.MSSQL.ErrorSpec (spec) where

import Hasura.Backends.MSSQL.SQL.Error
import Hasura.Prelude
import Test.Hspec

spec :: SpecWith a
spec = do
  it "test parseErrorClass all classes" $ const $ mapM_ testParseErrorClass testCases
  it "test parseErrorClass invalid SQLSTATE" $ const $ (parseErrorClass "99999") `shouldBe` Nothing
  where
    testParseErrorClass :: (String, ErrorClass) -> Expectation
    testParseErrorClass (sqlStateCode, expectedClass) =
      (parseErrorClass sqlStateCode) `shouldBe` (Just expectedClass)

testCases :: [(String, ErrorClass)]
testCases =
  [ ("22000", DataException NoSubclass),
    ("22001", DataException (Subclass StringDataRightTruncated)),
    ("22003", DataException (Subclass NumericValueOutOfRange)),
    ("22007", DataException (Subclass InvalidDatetimeFormat)),
    ("22008", DataException (Subclass DatetimeFieldOverflow)),
    ("22015", DataException (Subclass IntervalFieldOverflow)),
    ("22019", DataException (Subclass InvalidEscapeCharacter)),
    ("22025", DataException (Subclass InvalidEscapeSequence)),
    ("23000", IntegrityConstraintViolation),
    ("42000", SyntaxErrorOrAccessViolation NoSubclass),
    ("42S01", SyntaxErrorOrAccessViolation (Subclass TableOrViewAlreadyExists)),
    ("42S02", SyntaxErrorOrAccessViolation (Subclass TableOrViewNotFound)),
    ("42S11", SyntaxErrorOrAccessViolation (Subclass IndexAlreadyExists)),
    ("42S12", SyntaxErrorOrAccessViolation (Subclass IndexNotFound)),
    ("42S21", SyntaxErrorOrAccessViolation (Subclass ColumnAlreadyExists)),
    ("42S22", SyntaxErrorOrAccessViolation (Subclass ColumnNotFound))
  ]
