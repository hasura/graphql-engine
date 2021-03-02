module Hasura.Backends.MSSQL.DDL.RunSQL
  ( runSQL
  , MSSQLRunSQL
  )
where

import           Hasura.Prelude

import qualified Data.Aeson                       as J
import qualified Data.Text                        as T
import qualified Database.ODBC.Internal           as ODBC

import           Data.Aeson.TH
import           Data.String                      (fromString)

import           Hasura.Backends.MSSQL.Connection
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Schema            (RunSQLRes (..))
import           Hasura.RQL.Types


odbcValueToJValue :: ODBC.Value -> J.Value
odbcValueToJValue = \case
  ODBC.TextValue t       -> J.String t
  ODBC.ByteStringValue b -> J.String $ bsToTxt b
  ODBC.BinaryValue b     -> J.String $ bsToTxt $ ODBC.unBinary b
  ODBC.BoolValue b       -> J.Bool b
  ODBC.DoubleValue d     -> J.toJSON d
  ODBC.FloatValue f      -> J.toJSON f
  ODBC.IntValue i        -> J.toJSON i
  ODBC.ByteValue b       -> J.toJSON b
  ODBC.DayValue d        -> J.toJSON d
  ODBC.TimeOfDayValue td -> J.toJSON td
  ODBC.LocalTimeValue l  -> J.toJSON l
  ODBC.NullValue         -> J.Null

data MSSQLRunSQL
  = MSSQLRunSQL
  { _mrsSql    :: Text
  , _mrsSource :: !SourceName
  } deriving (Show, Eq)
$(deriveJSON hasuraJSON ''MSSQLRunSQL)

runSQL
  :: (MonadIO m, CacheRWM m, MonadError QErr m, MetadataM m)
  => MSSQLRunSQL -> m EncJSON
runSQL (MSSQLRunSQL sqlText source) = do
  pool <- _mscConnectionPool <$> askSourceConfig source
  results <- withMSSQLPool pool $ \conn -> ODBC.query conn $ fromString $ T.unpack sqlText
  pure $ encJFromJValue $ toResult results

toResult :: [[(ODBC.Column, ODBC.Value)]] -> RunSQLRes
toResult result = case result of
  []           -> RunSQLRes "CommandOk" J.Null
  (firstRow:_) -> RunSQLRes "TuplesOk" $ J.toJSON $ toHeader firstRow : toRows result
  where
    toRows   = map $ map $ odbcValueToJValue . snd
    toHeader = map $ J.String . ODBC.columnName . fst
