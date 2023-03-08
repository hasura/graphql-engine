{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

-- | Quasiquotation for 'Language.GraphQL.Draft.Syntax' types.
--
-- These quasiquoters can be used to construct GraphQL literal values at
-- compile-time.
module Language.GraphQL.Draft.Syntax.QQ
  ( name,
    executableDoc,
  )
where

-------------------------------------------------------------------------------

import Data.Text qualified as Text
import Language.GraphQL.Draft.Parser (parseExecutableDoc)
import Language.GraphQL.Draft.Syntax qualified as Syntax
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Prelude

-------------------------------------------------------------------------------

-- | Construct 'Syntax.Name' literals at compile-time via quasiquotation.
--
-- For example:
--
-- @
-- [name|foo_bar|]
-- @
--
-- ... would produce a 'Syntax.Name' value with the value @foo_bar@.
name :: QuasiQuoter
name =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "'name' does not support quoting patterns"
    quoteType _ = error "'name' does not support quoting types"
    quoteDec _ = error "'name' does not support quoting declarations"
    quoteExp str = case Syntax.mkName (Text.pack str) of
      Nothing -> error $ str <> " is not a valid GraphQL Name"
      Just result -> [|result|]

-- | Construct @'Syntax.ExecutableDocument' 'Syntax.Name'@ literals at compile
-- time via quasiquotation.
--
-- For example:
--
-- @
-- [executableDoc|
-- {
--   hero {
--     name
--     age
--   }
-- }
-- |]
-- @
executableDoc :: QuasiQuoter
executableDoc =
  QuasiQuoter {quoteExp, quotePat, quoteType, quoteDec}
  where
    quotePat _ = error "'executableDoc' does not support quoting patterns"
    quoteType _ = error "'executableDoc' does not support quoting types"
    quoteDec _ = error "'executableDoc' does not support quoting declarations"
    quoteExp str = case parseExecutableDoc (Text.pack str) of
      Left err -> fail . show $ err
      Right doc -> [|doc|]
