-- | Translate from the DML to the TSql dialect.

module Hasura.SQL.Tsql.Translate where

import           Data.Functor.Identity
import           Data.Proxy
import qualified Hasura.RQL.DML.Select.Types as Ir
import qualified Hasura.SQL.DML as Ir
import           Hasura.SQL.Tsql.Types as Tsql
import           Prelude

newtype Translate a = Translate { runTranslate :: Identity a}
  deriving (Functor, Applicative)

fromSelect :: Proxy (Ir.AnnSelectG (Ir.AnnFieldsG Ir.SQLExp) Ir.SQLExp) -> Translate Tsql.Select
fromSelect _ = pure Tsql.Select

fromExpression :: Proxy Ir.SQLExp -> Translate Tsql.Expression
fromExpression _ = pure Tsql.Expression
