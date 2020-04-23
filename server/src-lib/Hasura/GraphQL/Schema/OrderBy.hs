module Hasura.GraphQL.Schema.OrderBy
  ( orderByExp
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.Extended  as M
import qualified Language.GraphQL.Draft.Syntax as G

import qualified Hasura.GraphQL.Parser         as P
import qualified Hasura.RQL.DML.Select         as RQL

import           Hasura.GraphQL.Parser         (FieldsParser, Kind (..), Parser, UnpreparedValue,
                                                mkParameter)
import           Hasura.GraphQL.Parser.Class
import           Hasura.GraphQL.Schema.Common  (qualifiedObjectToName)
import           Hasura.RQL.Types
import           Hasura.SQL.DML
import           Hasura.SQL.Types
import           Hasura.SQL.Value

-- | Corresponds to an object type for an order by.
--
-- > input table_order_by {
-- >   col1: order_by
-- >   col2: order_by
-- >   .     .
-- >   .     .
-- >   coln: order_by
-- >   obj-rel: <remote-table>_order_by
-- > }
orderByExp
  :: forall m n. (MonadSchema n m, MonadError QErr m)
  => QualifiedTable -> m (Parser 'Input n [RQL.AnnOrderByItemG UnpreparedValue])
orderByExp = P.memoize 'orderByExp \tableName -> do
  name <- qualifiedObjectToName tableName <&> (<> $$(G.litName "_order_by"))
  let description = G.Description $
        "Ordering options when selecting data from " <> tableName <<> "."
  -- FIXME: permissions
  pure $ P.object name (Just description) $ sequenceA $ concat
    [
    ]
