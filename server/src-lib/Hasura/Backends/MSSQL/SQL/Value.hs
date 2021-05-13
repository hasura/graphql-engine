module Hasura.Backends.MSSQL.SQL.Value where

import           Hasura.Prelude

import qualified Database.ODBC.SQLServer               as ODBC
import qualified Hasura.GraphQL.Execute.LiveQuery.Plan as LQP
import qualified Hasura.RQL.Types.Column               as RQL

import           Data.Text.Encoding                    (decodeUtf8)

import           Hasura.Backends.MSSQL.Types.Internal  (Expression (..), Value)
import           Hasura.SQL.Backend
import           Hasura.SQL.Types                      (toSQLTxt)
import           Hasura.SQL.Value                      (TxtEncodedVal (..))

txtEncodedVal :: Value -> TxtEncodedVal
txtEncodedVal ODBC.NullValue           = TENull
txtEncodedVal (ODBC.ByteStringValue b) = TELit $ decodeUtf8 b
txtEncodedVal (ODBC.TextValue t)       = TELit t
txtEncodedVal val                      = TELit $ toSQLTxt $ ValueExpression val

toTxtEncodedVal :: forall f. Functor f => f (RQL.ColumnValue 'MSSQL) -> LQP.ValidatedVariables f
toTxtEncodedVal = LQP.ValidatedVariables . fmap (txtEncodedVal . RQL.cvValue)
