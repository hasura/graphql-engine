-- | MSSQL SQL Value
--
-- Provide a function to translate from a column value to its literal (textual)
-- value specific to MSSQL. Used in the 'BackendExecute' instance.
module Hasura.Backends.MSSQL.SQL.Value (txtEncodedColVal) where

import Data.Text.Encoding (decodeUtf8)
import Data.Text.Extended
import Database.ODBC.SQLServer qualified as ODBC
import Hasura.GraphQL.Execute.Subscription.Plan ()
import Hasura.Prelude
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column qualified as RQL
import Hasura.RQL.Types.Metadata.Instances ()
import Hasura.SQL.Value (TxtEncodedVal (..))

txtEncodedVal :: ODBC.Value -> TxtEncodedVal
txtEncodedVal ODBC.NullValue = TENull
txtEncodedVal (ODBC.ByteStringValue b) = TELit $ decodeUtf8 b
txtEncodedVal (ODBC.TextValue t) = TELit t
txtEncodedVal val = TELit $ toTxt $ ODBC.toSql val

txtEncodedColVal :: RQL.ColumnValue 'MSSQL -> TxtEncodedVal
txtEncodedColVal = txtEncodedVal . RQL.cvValue
