module Hasura.Backends.MSSQL.DDL.RunSQL
  (runSQL)
where

import           Hasura.Prelude

import           Control.Exception
import           Data.String                  (fromString)

import qualified Data.Aeson                   as J
import qualified Data.Text                    as T
import qualified Database.ODBC.Internal       as ODBC

import           Hasura.Backends.MSSQL.Result
import           Hasura.Backends.MSSQL.Types
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Schema        (RunSQLRes (..))
import           Hasura.RQL.Types


runSQL
  :: (MonadIO m, CacheRWM m, MonadError QErr m)
  => MSSQLRunSQL -> m EncJSON
runSQL (MSSQLRunSQL sqlText source) = do
  connection    <- _mscConnection <$> askSourceConfig source
  resultsEither <- liftIO $ try $ ODBC.query connection $ fromString $ T.unpack sqlText
  case resultsEither of
    Left (e :: SomeException) -> throw400 Unexpected $ "unexpected exception while executing query: " <> tshow e
    Right results -> pure $ encJFromJValue $ toResult results

toResult :: [[(ODBC.Column, ODBC.Value)]] -> RunSQLRes
toResult result = case result of
  []           -> RunSQLRes "CommandOk" J.Null
  (firstRow:_) -> RunSQLRes "TuplesOk" $ J.toJSON $ toHeader firstRow : toRows result
  where
    toRows   = map $ map $ odbcValueToJValue . snd
    toHeader = map $ J.String . ODBC.columnName . fst
