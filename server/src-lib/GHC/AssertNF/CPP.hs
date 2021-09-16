{-# LANGUAGE CPP #-}

-- | GHC.AssertNF.CPP localizes our use of CPP around calls
-- to 'assertNFHere', primarily to give tooling (e.g. ormolu)
-- an easier time.

module GHC.AssertNF.CPP
  ( assertNFHere
  , disableAssertNF
  , GHC.AssertNF.assertNFNamed
  )
  where

import qualified GHC.AssertNF
import           Hasura.Prelude
import           Language.Haskell.TH
#ifndef PROFILING
import           Text.Printf         (printf)
#endif

assertNFHere :: Q Exp
#ifdef PROFILING
assertNFHere = [| const (return ()) |]
#else
-- This is a copy of 'GHC.AssertNF.assertNFHere'. We can't easily
-- use the original because that relies on an import of "GHC.AssertNF".
-- Instead, we rewrite it to use the re-exported 'assertNFNamed'.
assertNFHere = do
    locStr <- formatLoc <$> location
    return $ AppE (VarE (mkName "GHC.AssertNF.CPP.assertNFNamed"))
                  (LitE (StringL locStr))
  where formatLoc :: Loc -> String
        formatLoc loc = let file = loc_filename loc
                            (line, col) = loc_start loc
                        in  printf "parameter at %s:%d:%d" file line col
#endif

disableAssertNF :: IO ()
#ifdef PROFILING
disableAssertNF = return ()
#else
disableAssertNF = GHC.AssertNF.disableAssertNF
#endif
