{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.Instances where

import           Hasura.Prelude

import           Instances.TH.Lift          ()
import qualified Language.Haskell.TH.Syntax as TH

import qualified Data.HashMap.Strict        as M

instance (TH.Lift k, TH.Lift v) => TH.Lift (M.HashMap k v) where
  lift m = [| M.fromList $(TH.lift $ M.toList m) |]
