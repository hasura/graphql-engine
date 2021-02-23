module Hasura.Backends.MSSQL.Result where

import           Hasura.Prelude

import qualified Data.Aeson             as J
import qualified Database.ODBC.Internal as ODBC


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
