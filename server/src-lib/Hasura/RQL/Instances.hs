{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hasura.RQL.Instances where

import           Hasura.Prelude

import           Instances.TH.Lift          ()
import qualified Language.Haskell.TH.Syntax as TH

import qualified Data.Aeson                 as J
import qualified Data.HashMap.Strict        as M
import qualified Data.HashMap.Strict.InsOrd as OMap
import qualified Data.HashSet               as S
import qualified Database.PostgreSQL.LibPQ  as PQ (Oid (..))
import           Foreign.C.Types
import           Data.Hashable
import qualified Database.PG.Query          as Q
import qualified PostgreSQL.Binary.Decoding as PD


instance (TH.Lift k, TH.Lift v) => TH.Lift (M.HashMap k v) where
  lift m = [| M.fromList $(TH.lift $ M.toList m) |]

instance (TH.Lift k, TH.Lift v) => TH.Lift (OMap.InsOrdHashMap k v) where
  lift m = [| OMap.fromList $(TH.lift $ OMap.toList m) |]

instance TH.Lift a => TH.Lift (S.HashSet a) where
  lift s = [| S.fromList $(TH.lift $ S.toList s) |]

instance J.FromJSON PQ.Oid where
  parseJSON v = fmap (PQ.Oid . CUInt) $ J.parseJSON v

instance J.ToJSON PQ.Oid where
  toJSON (PQ.Oid (CUInt i)) = J.toJSON i

instance Hashable PQ.Oid where
  hashWithSalt s (PQ.Oid (CUInt w)) = hashWithSalt s w

instance TH.Lift PQ.Oid where
  lift (PQ.Oid (CUInt w)) = [| PQ.Oid (CUInt $(TH.lift w)) |]

instance Q.FromCol PQ.Oid where
  fromCol x = fmap (PQ.Oid . CUInt) $ Q.fromColHelper PD.int x

