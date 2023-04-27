{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A convenience wrapper around "GHC.Stats", which makes RTS stats available
-- (when the program is run with +RTS -T)
module GHC.Stats.Extended
  ( module GHC.Stats,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.TH qualified as J
import GHC.Stats

$(J.deriveToJSON J.defaultOptions ''GCDetails)
$(J.deriveToJSON J.defaultOptions ''RTSStats)

{- for base >= 4.15
instance J.ToJSON S.GCDetails where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.ToJSON S.RTSStats where
    toEncoding = J.genericToEncoding J.defaultOptions
-}
