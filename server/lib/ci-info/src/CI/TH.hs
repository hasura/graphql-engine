{-# HLINT ignore "Use onLeft" #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CI.TH
  ( getVendors,
  )
where

import CI.Types qualified as Types
import Data.Aeson qualified as J
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH
import Prelude

getVendors :: TH.Q TH.Exp
getVendors = TH.runIO readVendors >>= TH.lift
  where
    vendorsPath = "res/vendors.json"

    readVendors :: IO [Types.Vendor]
    readVendors = do
      vendors <- J.eitherDecodeFileStrict' vendorsPath
      case vendors of
        Left e -> fail $ "parsing vendors.json failed: " <> e
        Right v -> return v
