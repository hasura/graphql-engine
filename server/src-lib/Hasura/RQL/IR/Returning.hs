{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.Returning
  ( MutFld,
    MutFldG (..),
    _MCount,
    _MExp,
    _MRet,
    MutFlds,
    MutFldsG,
    MutationOutput,
    MutationOutputG (..),
    buildEmptyMutResp,
    hasNestedFld,
  )
where

import Control.Lens.TH (makePrisms)
import Data.Aeson qualified as J
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Kind (Type)
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.IR.Select
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common

data MutFldG (b :: BackendType) (r :: Type) v
  = MCount
  | MExp Text
  | MRet (AnnFieldsG b r v)
  deriving stock (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Show (AnnFieldsG b r v)
  ) =>
  Show (MutFldG b r v)

deriving stock instance
  ( Backend b,
    Eq (AnnFieldsG b r v)
  ) =>
  Eq (MutFldG b r v)

type MutFld b = MutFldG b Void (SQLExpression b)

type MutFldsG b r v = Fields (MutFldG b r v)

data MutationOutputG (b :: BackendType) (r :: Type) v
  = MOutMultirowFields (MutFldsG b r v)
  | MOutSinglerowObject (AnnFieldsG b r v)
  deriving (Functor, Foldable, Traversable)

deriving stock instance
  ( Backend b,
    Show (MutFldsG b r v),
    Show (AnnFieldsG b r v)
  ) =>
  Show (MutationOutputG b r v)

deriving stock instance
  ( Backend b,
    Eq (MutFldsG b r v),
    Eq (AnnFieldsG b r v)
  ) =>
  Eq (MutationOutputG b r v)

type MutationOutput b = MutationOutputG b Void (SQLExpression b)

type MutFlds b = MutFldsG b Void (SQLExpression b)

buildEmptyMutResp :: MutationOutput backend -> EncJSON
buildEmptyMutResp = \case
  MOutMultirowFields mutFlds -> encJFromJValue $ InsOrdHashMap.fromList $ map (second convMutFld) mutFlds
  MOutSinglerowObject _ -> encJFromJValue $ J.Object mempty
  where
    convMutFld = \case
      MCount -> J.toJSON (0 :: Int)
      MExp e -> J.toJSON e
      MRet _ -> J.toJSON ([] :: [J.Value])

hasNestedFld :: MutationOutputG backend r a -> Bool
hasNestedFld = \case
  MOutMultirowFields flds -> any isNestedMutFld flds
  MOutSinglerowObject annFlds -> any isNestedAnnField annFlds
  where
    isNestedMutFld (_, mutFld) = case mutFld of
      MRet annFlds -> any isNestedAnnField annFlds
      _ -> False
    isNestedAnnField (_, annFld) = case annFld of
      AFObjectRelation _ -> True
      AFArrayRelation _ -> True
      _ -> False

$(makePrisms ''MutFldG)
