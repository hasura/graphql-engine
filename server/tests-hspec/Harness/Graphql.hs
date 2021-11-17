-- | Simple GRAPHQL quasi quoter. Even if this doesn't do anything,
-- it's still useful. Some editors (Emacs) display [graphql| ...|]
-- with GraphQL syntax highlighting. In the future, it could do some
-- basic validation with the graphql parser.
module Harness.Graphql (graphql) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Prelude

graphql :: QuasiQuoter
graphql =
  QuasiQuoter
    { quoteExp = stringE,
      quotePat = \_ -> fail "invalid",
      quoteType = \_ -> fail "invalid",
      quoteDec = \_ -> fail "invalid"
    }
