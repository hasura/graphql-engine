-- | Tests for Tsql conversion.

module Main where

import Data.Functor.Identity
import Data.Proxy
import Hasura.SQL.Tsql.Translate
import Hasura.SQL.Tsql.Types as Tsql
import Prelude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec =
  describe
    "Compile check"
    (it
       "Sanity"
       (shouldBe (runIdentity (runTranslate (fromSelect Proxy))) Tsql.Select))
