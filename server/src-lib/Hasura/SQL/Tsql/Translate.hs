-- | Translate from the DML to the TSql dialect.

module Hasura.SQL.Tsql.Translate where

import           Data.Proxy
import qualified Hasura.RQL.DML.Select.Types as Ir
import qualified Hasura.SQL.DML as Ir
import           Hasura.SQL.Tsql.Types as Tsql

fromSelect :: Proxy (Ir.AnnSelectG (Ir.AnnFieldsG Ir.SQLExp) Ir.SQLExp) -> Tsql.Select
fromSelect _ = Tsql.Select
