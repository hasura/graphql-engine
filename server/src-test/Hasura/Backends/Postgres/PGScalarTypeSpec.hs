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
  describe "parse array scalar types with arrays switched on" $ do
    let expectations =
          [ ("text", PGText),
            ("text[]", PGArray PGText),
            ("TEXT[]", PGArray PGText),
            ("some_enum[]", PGArray (PGUnknown "some_enum")),
            ("ST_GeomFromEWKT('SRID=4326;POINT(1 2)')", PGUnknown ("ST_GeomFromEWKT('SRID=4326;POINT(1 2)')"))
          ]

    traverse_
      ( \(txt, scalarType) ->
          it ("successfully parses " <> T.unpack txt) $ do
            textToPGScalarType txt `shouldBe` scalarType
      )
      expectations
