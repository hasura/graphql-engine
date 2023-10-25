module Database.MSSQL.TransactionSpec (spec) where

import Control.Exception.Base (bracket)
import Data.ByteString (ByteString)
import Database.MSSQL.Pool
import Database.MSSQL.Transaction
import Database.ODBC.SQLServer as ODBC
  ( ODBCException (DataRetrievalError, UnsuccessfulReturnCode),
    Query,
  )
import Hasura.Prelude
import Test.Hspec

-- | Describe a TransactionSpec test, see 'runTest' for additional details.
data TestCase a = TestCase
  { -- | Specifies which transactions to run. They will be executed sequentially,
    -- and the value of the last query in the last transaction will be compared
    -- against 'expectation'.
    --
    -- Has to be non-empty (but kept as a list for simplicity/convenience).
    transactions :: [Transaction],
    -- | Expected result of the test. 'Right' represents a successful outcome.
    --
    -- Left is presented as a function because we want to be able to partially
    -- match error (messages).
    expectation :: Either (MSSQLTxError -> Expectation) a,
    -- | Which kind of parser to use on the last query of the last transaction:
    --
    --   * 'unitQuery' for queries returning '()'
    --   * 'singleRowQuery' for queries returning 'a'
    --   * 'multiRowQuery' for queries returning '[a]'
    --
    -- Use 'TypeApplications' to specify the return type (needed to disambiguate
    -- the 'a' type parameter when needed).
    runWith :: Query -> TxT IO a,
    -- | Description for the test, used in the test output.
    description :: String
  }

newtype Transaction = Transaction
  { unTransaction :: [Query]
  }

spec :: SpecWith ConnectionString
spec = do
  runBasicChecks
  transactionStateTests

runBasicChecks :: SpecWith ConnectionString
runBasicChecks =
  describe "runTx transaction basic checks" $ do
    run
      TestCase
        { transactions =
            [ Transaction
                [ "CREATE TABLE SingleCol (ID INT)",
                  "INSERT INTO SingleCol VALUES (2)",
                  "SELECT ID FROM SingleCol"
                ]
            ],
          expectation = Right 2,
          runWith = singleRowQuery @Int,
          description = "CREATE, INSERT, SELECT single column"
        }

    run
      TestCase
        { transactions =
            [ Transaction
                [ "CREATE TABLE MultiCol (ID INT, NAME VARCHAR(1))",
                  "INSERT INTO MultiCol VALUES (2, 'A')",
                  "SELECT ID, NAME FROM MultiCol"
                ]
            ],
          expectation = Right (2, "A"),
          runWith = singleRowQuery @(Int, ByteString),
          description = "CREATE, INSERT, SELECT single multiple columns"
        }

    run
      TestCase
        { transactions = [Transaction ["SELECT 'hello'"]],
          expectation = matchDataRetrievalError "Expected Int, but got: ByteStringValue \"hello\"",
          runWith = singleRowQuery @Int,
          description = "SELECT the wrong type"
        }

    run
      TestCase
        { transactions = [Transaction ["select * from (values (1), (2)) as x(a)"]],
          expectation = Right [1, 2],
          runWith = multiRowQuery @Int,
          description = "SELECT multiple rows"
        }

    run
      TestCase
        { transactions = [Transaction ["select * from (values (1), (2)) as x(a)"]],
          expectation = matchDataRetrievalError "expecting single row",
          runWith = singleRowQuery @Int,
          description = "SELECT multiple rows, expect single row"
        }

    run
      TestCase
        { transactions =
            [ Transaction
                [ "CREATE TABLE BadQuery (ID INT, INVALID_SYNTAX)",
                  "INSERT INTO BadQuery VALUES (3)"
                ]
            ],
          expectation =
            matchQueryError
              (UnsuccessfulReturnCode "odbc_SQLExecDirectW" (-1) invalidSyntaxError (Just "42000")),
          runWith = unitQuery,
          description = "Bad syntax error/transaction rollback"
        }

-- | Test COMMIT and ROLLBACK for Active and NoActive states.
--
-- The Uncommittable state can be achieved by running the transaction enclosed
-- in a TRY..CATCH block, which is not currently doable with our current API.
-- Consider changing the API to allow such a test if we ever end up having
-- bugs because of it.
transactionStateTests :: SpecWith ConnectionString
transactionStateTests =
  describe "runTx Transaction State -> Action" $ do
    run
      TestCase
        { transactions = [Transaction ["SELECT 1"]],
          expectation = Right 1,
          runWith = singleRowQuery @Int,
          description = "Active -> COMMIT"
        }

    run
      TestCase
        { transactions =
            [ Transaction
                [ "CREATE TABLE SingleCol (ID INT)",
                  "INSERT INTO SingleCol VALUES (2)"
                ],
              Transaction -- Fail
                [ "CREATE TABLE BadQuery (ID INT, INVALID_SYNTAX)",
                  "UPDATE SingleCol SET ID=3"
                ],
              Transaction ["SELECT ID FROM SingleCol"] -- Grab data from setup
            ],
          expectation = Right 2,
          runWith = singleRowQuery @Int,
          description = "Active -> ROLLBACK"
        }

    run
      TestCase
        { transactions =
            [ Transaction -- Fail
                ["COMMIT; SELECT 1"]
            ],
          expectation =
            Left
              (`shouldBe` MSSQLInternal "No active transaction exist; cannot commit"),
          runWith = singleRowQuery @Int,
          description = "NoActive -> COMMIT"
        }
    run
      TestCase
        { transactions =
            [ Transaction
                [ "COMMIT;",
                  "CREATE TABLE BadQuery (ID INT, INVALID_SYNTAX)"
                ]
            ],
          -- We should get the error rather than the cannot commit error from the
          -- NoActive -> Commit test.
          expectation =
            matchQueryError
              (UnsuccessfulReturnCode "odbc_SQLExecDirectW" (-1) invalidSyntaxError (Just "42000")),
          runWith = unitQuery,
          description = "NoActive -> ROLLBACK"
        }

-- | Run a 'TestCase' by executing the queries in order. The last 'ODBC.Query'
-- is the one we check he result against.
--
-- Beacuse we don't know the type of the result, we need it supplied as part
-- of the 'TestCase':
--
--   * 'unitQuery' for queries returning '()'
--   * 'singleRowQuery' for queries returning 'a'
--   * 'multiRowQuery' for queries returning '[a]'
--
-- Note that we need to use TypeApplications on the 'runWith' function for type
-- checking to work, especially if the values are polymorphic
-- (e.g. numbers or strings).
--
-- Please also note that we are discarding 'Left's from "setup" transactions
-- (all but the last transaction). See the 'runSetup' helper below.
run :: forall a. (Eq a) => (Show a) => TestCase a -> SpecWith ConnectionString
run TestCase {..} =
  it description \connString -> do
    case reverse transactions of
      [] -> expectationFailure "Empty transaction list: nothing to do."
      (mainTransaction : leadingTransactions) -> do
        -- Run all transactions before the last (main) transaction.
        runSetup connString (reverse leadingTransactions)
        -- Get the result from the last transaction.
        result <-
          runInConn connString
            $ runQueries runWith
            $ unTransaction mainTransaction
        case (result, expectation) of
          -- Validate the error is the one we were expecting.
          (Left err, Left expected) ->
            expected err
          -- Verify the success result is the expected one.
          (Right res, Right expected) ->
            res `shouldBe` expected
          -- Expected success but got error. Needs special case because the expected
          -- Left is a validator (function).
          (Left err, Right expected) ->
            expectationFailure
              $ "Expected "
              <> show expected
              <> " but got error: "
              <> show err
          -- Expected error but got success. Needs special case because the expected
          -- Left is a validator (function).
          (Right res, Left _) ->
            expectationFailure
              $ "Expected error but got success: "
              <> show res
  where
    runSetup :: ConnectionString -> [Transaction] -> IO ()
    runSetup _ [] = pure ()
    runSetup connString (t : ts) = do
      -- Discards 'Left's.
      _ <- runInConn connString (runQueries unitQuery $ unTransaction t)
      runSetup connString ts

    runQueries :: (Query -> TxT IO x) -> [Query] -> TxT IO x
    runQueries _ [] = error $ "Expected at least one query per transaction in " <> description
    runQueries f [q] = f q
    runQueries f (x : xs) = unitQuery x *> runQueries f xs

-- | spec helper functions
runInConn :: ConnectionString -> TxT IO a -> IO (Either MSSQLTxError a)
runInConn connString query =
  bracket
    (createMinimalPool connString)
    drainMSSQLPool
    (runExceptT . runTx ReadCommitted query)

createMinimalPool :: ConnectionString -> IO MSSQLPool
createMinimalPool connString =
  initMSSQLPool connString
    $ ConnectionOptionsPool
    $ PoolOptions
      { poConnections = 1,
        poStripes = 1,
        poIdleTime = 5
      }

invalidSyntaxError :: String
invalidSyntaxError =
  "[Microsoft][ODBC Driver 18 for SQL Server][SQL Server]The definition for column 'INVALID_SYNTAX' must include a data type."

matchDataRetrievalError :: String -> Either (MSSQLTxError -> Expectation) a
matchDataRetrievalError = matchQueryError . DataRetrievalError

matchQueryError :: ODBCException -> Either (MSSQLTxError -> Expectation) a
matchQueryError expectedErr = Left $ \case
  MSSQLQueryError _ err -> err `shouldBe` expectedErr
  MSSQLConnError _ -> expectationFailure unexpectedMSSQLConnError
  MSSQLInternal _ -> expectationFailure unexpectedMSSQLInternalError

unexpectedMSSQLInternalError :: String
unexpectedMSSQLInternalError =
  "Expected MSSQLQueryError, but got: MSSQLInternal"

unexpectedMSSQLConnError :: String
unexpectedMSSQLConnError =
  "Expected MSSQLQueryError, but got: MSSQLConnError"
