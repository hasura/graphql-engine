module Hasura.GraphQL.Schema.Insert where

import           Hasura.Prelude

import qualified Hasura.RQL.DML.Insert.Types    as RQL
import qualified Hasura.RQL.DML.Returning.Types as RQL

import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Types


-- WIP NOTE
--
-- An abstract representation of insert was tricky to pin down. The
-- following structures are taken from Resolve, and slightly
-- modified. What needs to be decided is where those should go and
-- what we should do with them, really.
--
-- They are currently in this file in order to be accessible from the
-- Context file without creating a dependency loop, but this is only a
-- temporary emplacement: they are not part of the Schema and don't
-- belong in this folder.


data AnnIns a v
  = AnnIns
  { _aiInsObj         :: !a
  , _aiConflictClause :: !(Maybe (RQL.ConflictClauseP1 v))
  , _aiCheckCond      :: !(AnnBoolExp v, AnnBoolExp v)
  , _aiTableCols      :: ![PGColumnInfo]
  , _aiDefVals        :: !(PreSetColsG v)
  } deriving (Show, Eq)

type SingleObjIns v = AnnIns (AnnInsObj v) v
type MultiObjIns  v = AnnIns [AnnInsObj v] v

data RelIns a
  = RelIns
  { _riAnnIns  :: !a
  , _riRelInfo :: !RelInfo
  } deriving (Show, Eq)

type ObjRelIns v = RelIns (SingleObjIns v)
type ArrRelIns v = RelIns (MultiObjIns  v)

data AnnInsObj v
  = AnnInsObj
  { _aioColumns :: ![(PGCol, v)]
  , _aioObjRels :: ![ObjRelIns v]
  , _aioArrRels :: ![ArrRelIns v]
  } deriving (Show, Eq)

type AnnSingleInsert v = (SingleObjIns v, RQL.MutationOutputG v)
type AnnMultiInsert  v = (MultiObjIns  v, RQL.MutationOutputG v)

instance Semigroup (AnnInsObj v) where
  (AnnInsObj col1 obj1 rel1) <> (AnnInsObj col2 obj2 rel2) =
    AnnInsObj (col1 <> col2) (obj1 <> obj2) (rel1 <> rel2)

instance Monoid (AnnInsObj v) where
  mempty = AnnInsObj [] [] []


{-
traverseAnnInsert
  :: (Applicative f)
  => (a -> f b)
  -> AnnMultiInsert a
  -> f (AnnMultiInsert b)
traverseAnnInsert f (annIns, mutationOutput) = (,)
  <$> traverseMulti f annIns
  <*> traverseMutationOutput f mutOutput
  where
    traverseMulti f (AnnIns objs conflictClause checkCond columns defaultValues) = AnnIns
      <$> traverse (traverseObject f) objs
      <*> traverse (traverseConflictClause f) conflictClause
      <*> ( traverseAnnBoolExp f $ fst checkCond
          , traverseAnnBoolExp f $ snd checkCond
          )
      <*> pure columns
      <*> traverse f defaultValues
    traverseSingle f (AnnIns obj conflictClause checkCond columns defaultValues) = AnnIns
      <$> traverseObject f obj
      <*> traverse (traverseConflictClause f) conflictClause
      <*> ( traverseAnnBoolExp f $ fst checkCond
          , traverseAnnBoolExp f $ snd checkCond
          )
      <*> pure columns
      <*> traverse f defaultValues
    traverseObject f (AnnInsObj columns objRels arrRels) = AnnInsObj
      <$> traverse (traverse f) columns
      <*> traverse (traverseRel $ traverseSingle f) objRels
      <*> traverse (traverseRel $ traverseMulti  f) arrRels
    traverseRel f (RelIns object relInfo) = RelIns <$> f object <*> pure relInfo
-}
