module Database.MSSQL.TransactionSpec (spec) where

import Control.Exception.Base (bracket)
import Data.ByteString
import Database.MSSQL.Transaction
import Database.ODBC.SQLServer as ODBC
  ( ODBCException (DataRetrievalError, UnsuccessfulReturnCode),
    close,
    connect,
  )
import Hasura.Prelude
import Test.Hspec

spec :: Text -> Spec
spec connString = do
  describe "runTx" $ do
    it "runs command in a transaction" $ do
      result <- runInConn connString selectQuery
      result `shouldBe` Right 1

    it "commits a successful transaction, returning a single field" $ do
      _ <- runInConn connString insertIdQuery
      result <- runInConn connString selectIdQuery
      result `shouldBe` Right 2

    it "commits a successful transaction, returning multiple fields" $ do
      _ <- runInConn connString insertNameQuery
      result <- runInConn connString selectIdNameQuery
      result `shouldBe` Right (2, "A")

    it "an unsuccesful transaction, expecting Int" $ do
      result <- runInConn connString selectIntQueryFail
      either
        (\(MSSQLTxError _ err) -> err `shouldBe` DataRetrievalError "Expected Int, but got: ByteStringValue \"hello\"")
        (\r -> expectationFailure $ "expected Left, returned " <> show r)
        result

    it "a successfull query expecting multiple rows" $ do
      result <- runInConn connString selectMultipleIdQuery
      result `shouldBe` Right [1, 2]

    it "an unsuccesful transaction; expecting single row" $ do
      result <- runInConn connString selectIdQueryFail
      either
        (\(MSSQLTxError _ err) -> err `shouldBe` DataRetrievalError "expecting single row")
        (\r -> expectationFailure $ "expected Left, returned " <> show r)
        result

    it "displays the SQL Server error on an unsuccessful transaction" $ do
      result <- runInConn connString badQuery
      either
        ( \(MSSQLTxError _ err) ->
            err
              `shouldBe` UnsuccessfulReturnCode "odbc_SQLExecDirectW" (-1) invalidSyntaxError
        )
        (\() -> expectationFailure "expected Left, returned ()")
        result

    it "rolls back an unsuccessful transaction" $ do
      _ <- runInConn connString badQuery
      result <- runInConn connString selectIdQuery
      either
        (\err -> expectationFailure $ "expected Right, returned " <> show err)
        (\value -> value `shouldNotBe` 3)
        result

selectQuery :: TxT IO Int
selectQuery = singleRowQuery "SELECT 1"

selectIntQueryFail :: TxT IO Int
selectIntQueryFail = singleRowQuery "SELECT 'hello'"

selectIdQueryFail :: TxT IO Int
selectIdQueryFail = singleRowQuery "select * from (values (1), (2)) as x(a)"

selectMultipleIdQuery :: TxT IO [Int]
selectMultipleIdQuery = multiRowQuery "select * from (values (1), (2)) as x(a)"

insertIdQuery :: TxT IO ()
insertIdQuery =
  unitQuery
    "CREATE TABLE SingleCol (ID INT);INSERT INTO SingleCol VALUES (2);"

selectIdQuery :: TxT IO Int
selectIdQuery =
  singleRowQuery
    "SELECT ID FROM SingleCol;"

insertNameQuery :: TxT IO ()
insertNameQuery =
  unitQuery
    "CREATE TABLE MultiCol (ID INT, NAME VARCHAR(1));INSERT INTO MultiCol VALUES (2, 'A');"

selectIdNameQuery :: TxT IO (Int, ByteString)
selectIdNameQuery =
  singleRowQuery
    "SELECT ID, NAME FROM MultiCol;"

badQuery :: TxT IO ()
badQuery =
  unitQuery
    "CREATE TABLE BadQuery (ID INT, INVALID_SYNTAX);INSERT INTO BadQuery VALUES (3);"

-- | spec helper functions
runInConn :: Text -> TxT IO a -> IO (Either MSSQLTxError a)
runInConn connString query =
  bracket
    (connect connString)
    close
    (runExceptT . runTx query)

invalidSyntaxError :: String
invalidSyntaxError =
  "[Microsoft][ODBC Driver 17 for SQL Server][SQL Server]The definition for column 'INVALID_SYNTAX' must include a data type.[Microsoft][ODBC Driver 17 for SQL Server][SQL Server]The definition for column 'INVALID_SYNTAX' must include a data type."
