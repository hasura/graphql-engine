module Hasura.RQL.IR.Returning where

import           Hasura.Prelude

import qualified Data.Aeson                       as J
import qualified Data.HashMap.Strict.InsOrd       as OMap

import           Hasura.EncJSON
import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend

import qualified Hasura.Backends.Postgres.SQL.DML as S


data MutFldG (b :: Backend) v
  = MCount
  | MExp !Text
  | MRet !(AnnFieldsG b v)

type MutFld b = MutFldG b (SQLExp b)

type MutFldsG b v = Fields (MutFldG b v)

data MutationOutputG (b :: Backend) v
  = MOutMultirowFields !(MutFldsG b v)
  | MOutSinglerowObject !(AnnFieldsG b v)

type MutationOutput b = MutationOutputG b (SQLExp b)

type MutFlds b = MutFldsG b (SQLExp b)

buildEmptyMutResp :: MutationOutput backend -> EncJSON
buildEmptyMutResp = \case
  MOutMultirowFields mutFlds -> encJFromJValue $ OMap.fromList $ map (second convMutFld) mutFlds
  MOutSinglerowObject _      -> encJFromJValue $ J.Object mempty
  where
    convMutFld = \case
      MCount -> J.toJSON (0 :: Int)
      MExp e -> J.toJSON e
      MRet _ -> J.toJSON ([] :: [J.Value])

traverseMutFld
  :: (Applicative f)
  => (a -> f b)
  -> MutFldG backend a
  -> f (MutFldG backend b)
traverseMutFld f = \case
  MCount    -> pure MCount
  MExp t    -> pure $ MExp t
  MRet flds -> MRet <$> traverse (traverse (traverseAnnField f)) flds

traverseMutationOutput
  :: (Applicative f)
  => (a -> f b)
  -> MutationOutputG backend a -> f (MutationOutputG backend b)
traverseMutationOutput f = \case
  MOutMultirowFields mutationFields ->
    MOutMultirowFields <$> traverse (traverse (traverseMutFld f)) mutationFields
  MOutSinglerowObject annFields ->
    MOutSinglerowObject <$> traverseAnnFields f annFields

traverseMutFlds
  :: (Applicative f)
  => (a -> f b)
  -> MutFldsG backend a
  -> f (MutFldsG backend b)
traverseMutFlds f =
  traverse (traverse (traverseMutFld f))

hasNestedFld :: MutationOutputG backend a -> Bool
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

-- | The postgres common table expression (CTE) for mutation queries.
-- This CTE expression is used to generate mutation field output expression,
-- see Note [Mutation output expression].
data MutationCTE
  = MCCheckConstraint !S.CTE -- ^ A Mutation with check constraint validation (Insert or Update)
  | MCSelectValues !S.Select -- ^ A Select statement which emits mutated table rows
  | MCDelete !S.SQLDelete -- ^ A Delete statement
  deriving (Show, Eq)

getMutationCTE :: MutationCTE -> S.CTE
getMutationCTE = \case
  MCCheckConstraint cte -> cte
  MCSelectValues select -> S.CTESelect select
  MCDelete delete       -> S.CTEDelete delete

checkPermissionRequired :: MutationCTE -> Bool
checkPermissionRequired = \case
  MCCheckConstraint _ -> True
  MCSelectValues _    -> False
  MCDelete _          -> False
