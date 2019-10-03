{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.Instances where

import           Hasura.Prelude

import           Instances.TH.Lift          ()
import qualified Language.Haskell.TH.Syntax as TH

import qualified Data.HashMap.Strict        as M
import qualified Data.HashSet               as S

instance (TH.Lift k, TH.Lift v) => TH.Lift (M.HashMap k v) where
  lift m = [| M.fromList $(TH.lift $ M.toList m) |]

instance TH.Lift a => TH.Lift (S.HashSet a) where
  lift s = [| S.fromList $(TH.lift $ S.toList s) |]
