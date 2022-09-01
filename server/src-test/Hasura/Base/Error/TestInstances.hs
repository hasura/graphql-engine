{-# OPTIONS_GHC -Wno-orphans #-}

module Hasura.Base.Error.TestInstances () where

import Data.Text qualified as Text
import Hasura.Base.Error
import Hasura.Prelude

-- Orphan instance so that we can write assertions over 'Either QErr a'.
instance Show QErr where
  show = Text.unpack . showQErr
