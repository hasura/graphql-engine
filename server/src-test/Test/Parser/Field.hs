-- | QuasiQuoter for parsing GraphQL fields in tests. See 'field' for details.
module Test.Parser.Field (field) where

import Control.Monad.Trans.Except
import Data.Attoparsec.Text qualified as Parser
import Data.Text qualified as T
import Hasura.Base.Error (showQErr)
import Hasura.GraphQL.Execute.Inline (inlineField, runInlineM)
import Hasura.GraphQL.Parser.Variable
import Hasura.Prelude
import Language.GraphQL.Draft.Parser qualified as GraphQL
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Language.Haskell.TH.Lib (ExpQ)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax qualified as TH

-- | Quasi-Quoter for GraphQL fields.
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
    { quoteExp = fieldExp,
      quotePat = \_ -> fail "invalid",
      quoteType = \_ -> fail "invalid",
      quoteDec = \_ -> fail "invalid"
    }
  where
    fieldExp :: String -> ExpQ
    fieldExp input = do
      either fail TH.lift $
        runExcept $ do
          parsed <- hoistEither $ Parser.parseOnly (Parser.skipSpace *> GraphQL.field @GraphQL.Name) . T.pack $ input
          fixField parsed

    -- A parsed field can contain both fragments and variables.
    -- We support neither yet.
    fixField :: GraphQL.Field GraphQL.FragmentSpread GraphQL.Name -> Except String (GraphQL.Field GraphQL.NoFragments Variable)
    fixField f = do
      x <- except $ mapLeft (T.unpack . showQErr) $ runInlineM mempty . inlineField $ f
      traverse (throwE . ("Variables are not supported in tests yet: " ++) . show) x
