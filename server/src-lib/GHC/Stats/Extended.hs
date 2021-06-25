{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | A convenience wrapper around "GHC.Stats", which makes RTS stats available
-- (when the program is run with +RTS -T)
module GHC.Stats.Extended
  ( module GHC.Stats
  ) where

import qualified Data.Aeson    as A
import qualified Data.Aeson.TH as A
import           GHC.Stats

$(A.deriveToJSON A.defaultOptions ''GCDetails)
$(A.deriveToJSON A.defaultOptions ''RTSStats)

{- for base >= 4.15
instance A.ToJSON S.GCDetails where
    toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON S.RTSStats where
    toEncoding = A.genericToEncoding A.defaultOptions
-}
