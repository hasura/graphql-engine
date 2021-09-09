module Database.MSSQL.TransactionSpec (spec) where

import           Hasura.Prelude

import           Control.Exception.Base     (bracket)
import           Database.MSSQL.Transaction
import           Database.ODBC.SQLServer    as ODBC (ODBCException (UnsuccessfulReturnCode),
                                                     Value (ByteStringValue, IntValue), close,
                                                     connect)
import           Test.Hspec


spec :: Text -> Spec
spec connString = do
  describe "runTx" $ do
    it "runs command in a transaction" $ do
      result <- runInConn connString selectQuery
      result `shouldBe` Right (ResultOk [[IntValue 1]])

    it "commits a successful transaction, returning a single field" $ do
      _ <- runInConn connString insertIdQuery
      result <- runInConn connString selectIdQuery
      result `shouldBe` Right (ResultOk [[IntValue 2]])

    it "commits a successful transaction, returning multiple fields" $ do
      _ <- runInConn connString insertNameQuery
      result <- runInConn connString selectIdNameQuery
      result `shouldBe` Right (ResultOk [[IntValue 2,ByteStringValue "A"]])

    it "displays the SQL Server error on an unsuccessful transaction" $ do
      result <- runInConn connString badQuery
      either
        (\(MSSQLTxError _ err) -> err `shouldBe`
          UnsuccessfulReturnCode "odbc_SQLExecDirectW" (-1) invalidSyntaxError)
        (\(ResultOk resultOk) -> expectationFailure $ "expected Left, returned " <> show resultOk)
        result

    it "rolls back an unsuccessful transaction" $ do
      _ <- runInConn connString badQuery
      result <- runInConn connString selectIdQuery
      either
        (\err -> expectationFailure $ "expected Right, returned " <> show err)
        (\(ResultOk resultOk) -> resultOk `shouldNotContain` [[IntValue 3]])
        result

selectQuery :: TxT IO ResultOk
selectQuery = withQ "SELECT 1"

insertIdQuery :: TxT IO ResultOk
insertIdQuery = withQ
  "CREATE TABLE SingleCol (ID INT);INSERT INTO SingleCol VALUES (2);"

selectIdQuery :: TxT IO ResultOk
selectIdQuery = withQ
  "SELECT ID FROM SingleCol;"

insertNameQuery :: TxT IO ResultOk
insertNameQuery = withQ
  "CREATE TABLE MultiCol (ID INT, NAME VARCHAR(1));INSERT INTO MultiCol VALUES (2, 'A');"

selectIdNameQuery :: TxT IO ResultOk
selectIdNameQuery = withQ
  "SELECT ID, NAME FROM MultiCol;"

badQuery :: TxT IO ResultOk
badQuery = withQ
  "CREATE TABLE BadQuery (ID INT, INVALID_SYNTAX);INSERT INTO BadQuery VALUES (3);"

-- | spec helper functions
runInConn :: Text -> TxT IO ResultOk -> IO (Either MSSQLTxError ResultOk)
runInConn connString query =
  bracket
    (connect connString)
    close
    (runExceptT . runTx query)

invalidSyntaxError :: String
invalidSyntaxError =
  "[Microsoft][ODBC Driver 17 for SQL Server][SQL Server]The definition for column 'INVALID_SYNTAX' must include a data type.[Microsoft][ODBC Driver 17 for SQL Server][SQL Server]The definition for column 'INVALID_SYNTAX' must include a data type."
