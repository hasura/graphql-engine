-- | Planning T-SQL queries and subscriptions.

module Hasura.SQL.Tsql.Plan
  ( rootFieldToSelect
  ) where

import           Control.Applicative
import           Control.Monad.Validate
import           Data.Void
import qualified Hasura.GraphQL.Context as Graphql
import qualified Hasura.RQL.DML.Select as DS
import qualified Hasura.SQL.DML as Sql
import qualified Hasura.SQL.Tsql.FromIr as Tsql
import           Hasura.SQL.Tsql.Types as Tsql

--------------------------------------------------------------------------------
-- Converting a root field into a T-SQL select statement

-- TODO: Use 'alias'.
rootFieldToSelect :: Sql.Alias -> Graphql.RootField (Graphql.QueryDB Void) Void Void Void -> Tsql.FromIr Select
rootFieldToSelect _alias =
  \case
    Graphql.RFDB (Graphql.QDBPrimaryKey s) ->
      Tsql.mkSQLSelect DS.JASSingleObject s
    Graphql.RFDB (Graphql.QDBSimple s) -> Tsql.mkSQLSelect DS.JASMultipleRows s
    Graphql.RFDB (Graphql.QDBAggregation s) -> Tsql.fromSelectAggregate s
    Graphql.RFDB (Graphql.QDBConnection {}) ->
      refute (pure Tsql.ConnectionsNotSupported)

--------------------------------------------------------------------------------
-- Producing the correct SQL-level list comprehension to multiplex a query

-- Problem description:
--
--
-- Generate a query that repeats the same query N times but with
-- certain slots replaced:
--
-- [ Select x y | (x,y) <- [..] ]
--
{-
zsh> select row.*, result.*
  from openjson('[{"result_id":1,"result_vars":[23,"xyz"]},{"result_id":2,"result_vars":[52,"abc"]}]')
  with (result_id int, result_vars nvarchar(max) as json)
  as row
  outer apply (select Name from chinook.dbo.Artist where Artistid = json_value(result_vars,'$[0]'))
  as result;
[row 0]
result_id: 1
result_vars: "[23,\"xyz\"]"
Name: "Frank Zappa & Captain Beefheart"

[row 1]
result_id: 2
result_vars: "[52,\"abc\"]"
Name: "Kiss"

Rows: 2
-}
