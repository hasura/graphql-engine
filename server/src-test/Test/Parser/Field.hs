-- | QuasiQuoter for parsing GraphQL fields in tests. See 'field' for details.
module Test.Parser.Field (field) where

import Data.Attoparsec.Text qualified as Parser
import Data.Text qualified as T
import Hasura.GraphQL.Parser.Variable
import Hasura.Prelude
import Language.GraphQL.Draft.Parser qualified as GraphQL
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Language.Haskell.TH.Lib (ExpQ)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax qualified as TH
import Unsafe.Coerce (unsafeCoerce)

-- | Warning: we are currently using unsafe coercions to convert the field. This
-- seems to work for now, but beware.
--
-- Example usage:
-- > [GQL.field|
-- >    update_artist(
-- >      where: { name: { _eq: "old name"}},
-- >      _set: { name: "new name" }
-- >    ) {
-- >    affected_rows
-- >    }
-- > |],
field :: QuasiQuoter
field =
  QuasiQuoter
    { quoteExp = evalFieldGQL,
      quotePat = \_ -> fail "invalid",
      quoteType = \_ -> fail "invalid",
      quoteDec = \_ -> fail "invalid"
    }

evalFieldGQL :: String -> ExpQ
evalFieldGQL = either fail TH.lift . go
  where
    -- Note: @skipSpace@ is used here to allow trailing whitespace in the QQ.
    go :: String -> Either String (GraphQL.Field GraphQL.NoFragments Variable)
    go =
      fmap fixField
        . Parser.parseOnly (Parser.skipSpace *> GraphQL.field @GraphQL.Name)
        . T.pack

fixField :: GraphQL.Field GraphQL.FragmentSpread GraphQL.Name -> GraphQL.Field GraphQL.NoFragments Variable
fixField = unsafeCoerce
