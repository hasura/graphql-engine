module Hasura.Backends.Postgres.PGScalarTypeSpec
  ( spec,
  )
where

import Data.Foldable (traverse_)
import Data.Text qualified as T
import Hasura.Backends.Postgres.SQL.Types
import Test.Hspec
import Prelude

spec :: Spec
spec =
  describe "parse array scalar types" $ do
    let expectations =
          [ ("text", PGText),
            ("text[]", PGArray PGText),
            ("_text[]", PGUnknown "_text[]"),
            ("array text", PGArray PGText),
            ("TEXT", PGText),
            ("TEXT[]", PGArray PGText),
            ("_TEXT[]", PGUnknown "_TEXT[]"),
            ("ARRAY TEXT", PGArray PGText)
          ]

    traverse_
      ( \(txt, scalarType) ->
          it ("successfully parses " <> T.unpack txt) $ do
            textToPGScalarType txt `shouldBe` scalarType
      )
      expectations
