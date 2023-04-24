{-# LANGUAGE CPP #-}

-- | GHC.AssertNF.CPP localizes our use of CPP around calls
-- to 'assertNFHere', primarily to give tooling (e.g. ormolu)
-- an easier time.
--
-- We disable the 'assertNF'-related code because it is provided
-- by the package ghc-heap-view, which can't be built using profiling.

#ifdef PROFILING
{-# LANGUAGE TemplateHaskell #-}

module GHC.AssertNF.CPP
  ( assertNFHere,
    disableAssertNF,
  )
where

import Hasura.Prelude
import Language.Haskell.TH

assertNFHere :: Q Exp
assertNFHere = [| const (return ()) |]

disableAssertNF :: IO ()
disableAssertNF = return ()

#else

module GHC.AssertNF.CPP
  ( assertNFHere,
    disableAssertNF,
    GHC.AssertNF.assertNFNamed,
  )
where

import GHC.AssertNF qualified
import Hasura.Prelude
import Language.Haskell.TH
import Text.Printf (printf)

assertNFHere :: Q Exp
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

disableAssertNF :: IO ()
disableAssertNF = GHC.AssertNF.disableAssertNF

#endif
