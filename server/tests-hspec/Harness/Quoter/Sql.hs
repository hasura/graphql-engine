-- | Simple SQL quasi quoter. Even if this doesn't do anything, it's
-- still useful. Some editors (Emacs) display [sql| ...|] with SQL
-- syntax highlighting.
module Harness.Quoter.Sql (sql) where

import Hasura.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quoteExp = stringE,
      quotePat = \_ -> fail "invalid",
      quoteType = \_ -> fail "invalid",
      quoteDec = \_ -> fail "invalid"
    }
