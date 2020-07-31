module Hasura.CacheBoundedSpec (spec) where

import           Hasura.Prelude

import qualified Hasura.Cache.Bounded             as Cache
import           Test.Hspec

spec :: Spec
spec = describe "Bounded cache data structure" $ do
  -- assume a single stripe here for simplicity:
  let checkEntries c expected = do
        Cache.checkInvariants c
        Cache.getEntriesRecency c >>= \case
          [es] -> do 
            sort es `shouldBe` expected
          _ -> error "stripes wrong"

  it "works for 0 size" $ do
    c <- Cache.initialiseStripes 1 0
    Cache.lookup 'X' c `shouldReturn` Nothing
    Cache.insert 'Y' 'Y' c
    Cache.lookup 'Y' c `shouldReturn` Nothing
    checkEntries c []

  -- basic functionality check:
  it "seems to be working right" $ do
    c <- Cache.initialiseStripes 1 3

    Cache.insert 'A' 'A' c
    checkEntries c [('A', 0, 'A')]

    -- lookups of non-existing keys don't increment the ticket (though it
    -- wouldn't hurt if it did):
    Cache.lookup 'X' c `shouldReturn` Nothing

    Cache.lookup 'A' c `shouldReturn` Just 'A'
    checkEntries c [('A', 1, 'A')]

    Cache.insert 'B' 'B' c
    checkEntries c [('A', 1, 'A'), ('B', 2, 'B')]

    Cache.lookup 'B' c `shouldReturn` Just 'B'
    checkEntries c [('A', 1, 'A'), ('B', 3, 'B')]

    Cache.lookup 'A' c `shouldReturn` Just 'A'
    checkEntries c [('A', 4, 'A'), ('B', 3, 'B')]

    Cache.insert 'C' 'C' c
    checkEntries c [('A', 4, 'A'), ('B', 3, 'B'), ('C', 5, 'C')]

    -- evict 'B':
    Cache.insert 'D' 'D' c
    checkEntries c [('A', 4, 'A'), ('C', 5, 'C'), ('D', 6, 'D')]
    Cache.lookup 'B' c `shouldReturn` Nothing
