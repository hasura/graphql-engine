module Hasura.Backends.MSSQL.SQL.Value (txtEncodedColVal) where

import           Hasura.Prelude

import qualified Database.ODBC.SQLServer               as ODBC

import           Data.Text.Encoding                    (decodeUtf8)
import           Data.Text.Extended

import qualified Hasura.RQL.Types.Column               as RQL

import           Hasura.Backends.MSSQL.Types.Internal  (Value)
import           Hasura.GraphQL.Execute.LiveQuery.Plan ()
import           Hasura.SQL.Backend
import           Hasura.SQL.Value                      (TxtEncodedVal (..))


txtEncodedVal :: Value -> TxtEncodedVal
txtEncodedVal ODBC.NullValue           = TENull
txtEncodedVal (ODBC.ByteStringValue b) = TELit $ decodeUtf8 b
txtEncodedVal (ODBC.TextValue t)       = TELit t
txtEncodedVal val                      = TELit $ toTxt $ ODBC.toSql val

txtEncodedColVal :: RQL.ColumnValue 'MSSQL -> TxtEncodedVal
txtEncodedColVal = txtEncodedVal . RQL.cvValue
