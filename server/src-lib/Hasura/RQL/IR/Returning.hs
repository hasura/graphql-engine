module Hasura.RQL.IR.Returning where

import           Hasura.Prelude

import qualified Data.Aeson                 as J
import qualified Data.HashMap.Strict.InsOrd as OMap

import           Data.Kind                  (Type)

import           Hasura.EncJSON
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types.Backend
import           Hasura.SQL.Backend


data MutFldG (b :: BackendType) (r :: BackendType -> Type) v
  = MCount
  | MExp !Text
  | MRet !(AnnFieldsG b r v)
  deriving (Functor, Foldable, Traversable)

type MutFld b = MutFldG b (Const Void) (SQLExpression b)
type MutFldsG b r v = Fields (MutFldG b r v)

data MutationOutputG (b :: BackendType) (r :: BackendType -> Type) v
  = MOutMultirowFields  !(MutFldsG b r v)
  | MOutSinglerowObject !(AnnFieldsG b r v)
  deriving (Functor, Foldable, Traversable)

type MutationOutput b = MutationOutputG b (Const Void) (SQLExpression b)

type MutFlds b = MutFldsG b (Const Void) (SQLExpression b)

buildEmptyMutResp :: MutationOutput backend -> EncJSON
buildEmptyMutResp = \case
  MOutMultirowFields mutFlds -> encJFromJValue $ OMap.fromList $ map (second convMutFld) mutFlds
  MOutSinglerowObject _      -> encJFromJValue $ J.Object mempty
  where
    convMutFld = \case
      MCount -> J.toJSON (0 :: Int)
      MExp e -> J.toJSON e
      MRet _ -> J.toJSON ([] :: [J.Value])

hasNestedFld :: MutationOutputG backend r a -> Bool
hasNestedFld = \case
  MOutMultirowFields flds     -> any isNestedMutFld flds
  MOutSinglerowObject annFlds -> any isNestedAnnField annFlds
  where
    isNestedMutFld (_, mutFld) = case mutFld of
      MRet annFlds -> any isNestedAnnField annFlds
      _            -> False
    isNestedAnnField (_, annFld) = case annFld of
      AFObjectRelation _ -> True
      AFArrayRelation _  -> True
      _                  -> False
