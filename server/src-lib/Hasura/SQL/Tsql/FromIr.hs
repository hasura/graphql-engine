-- | Translate from the DML to the TSql dialect.

module Hasura.SQL.Tsql.FromIr where

import           Data.Functor.Identity
import           Data.Proxy
import qualified Hasura.RQL.DML.Select.Types as Ir
import qualified Hasura.SQL.DML as Ir
import           Hasura.SQL.Tsql.Types as Tsql
import           Prelude

newtype FromIr a = FromIr { runFromIr :: Identity a}
  deriving (Functor, Applicative)

fromSelect :: Ir.AnnSelectG (Ir.AnnFieldsG Ir.SQLExp) Ir.SQLExp -> FromIr (Proxy Tsql.Select)
fromSelect Ir.AnnSelectG {_asnFields, _asnFrom, _asnPerm, _asnArgs, _asnStrfyNum} =
  pure Proxy

fromExpression :: Proxy Ir.SQLExp -> FromIr (Proxy Tsql.Expression)
fromExpression _ = pure Proxy
