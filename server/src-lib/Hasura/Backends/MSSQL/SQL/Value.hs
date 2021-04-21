module Hasura.Backends.MSSQL.SQL.Value where

import           Hasura.Prelude

import           Hasura.Backends.MSSQL.Types.Internal  (Value)
import           Hasura.SQL.Value                      (TxtEncodedVal (..))

import qualified Database.ODBC.SQLServer               as ODBC
import qualified Hasura.GraphQL.Execute.LiveQuery.Plan as LQP
import qualified Hasura.RQL.Types.Column               as RQL

import           Hasura.SQL.Backend

txtEncodedVal :: Value -> TxtEncodedVal
txtEncodedVal ODBC.NullValue = TENull
txtEncodedVal val            = TELit $ tshow val

toTxtEncodedVal :: forall f. Functor f => f (RQL.ColumnValue 'MSSQL) -> LQP.ValidatedVariables f
toTxtEncodedVal = LQP.ValidatedVariables . fmap (txtEncodedVal . RQL.cvValue)
