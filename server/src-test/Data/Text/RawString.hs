module Data.Text.RawString (raw) where

import Hasura.Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote

raw :: QuasiQuoter
raw =
  QuasiQuoter
    { quoteExp = pure . LitE . StringL,
      quotePat = const $ failWith "pattern",
      quoteType = const $ failWith "type",
      quoteDec = const $ failWith "declaration"
    }
  where
    failWith t = fail $ "illegal raw string quote location; expected expresion, got " <> t
