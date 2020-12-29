{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Translate.Types where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                as HM

import qualified Hasura.Backends.Postgres.SQL.DML   as PG
import qualified Hasura.Backends.Postgres.SQL.Types as PG

import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend


data SourcePrefixes
  = SourcePrefixes
  { _pfThis :: !PG.Identifier -- ^ Current source prefix
  , _pfBase :: !PG.Identifier
  -- ^ Base table source row identifier to generate
  -- the table's column identifiers for computed field
  -- function input parameters
  } deriving (Show, Eq, Generic)
instance Hashable SourcePrefixes

data SelectSource (b :: BackendType)
  = SelectSource
  { _ssPrefix   :: !PG.Identifier
  , _ssFrom     :: !PG.FromItem
  , _ssDistinct :: !(Maybe PG.DistinctExpr)
  , _ssWhere    :: !PG.BoolExp
  , _ssOrderBy  :: !(Maybe PG.OrderByExp)
  , _ssLimit    :: !(Maybe Int)
  , _ssOffset   :: !(Maybe (SQLExpression b))
  } deriving (Generic)
instance Hashable (SelectSource 'Postgres)
deriving instance Show (SelectSource 'Postgres)
deriving instance Eq   (SelectSource 'Postgres)

data SelectNode (b :: BackendType)
  = SelectNode
  { _snExtractors :: !(HM.HashMap (Alias b) (SQLExpression b))
  , _snJoinTree   :: !(JoinTree b)
  }

instance Semigroup (SelectNode 'Postgres) where
  SelectNode lExtrs lJoinTree <> SelectNode rExtrs rJoinTree =
    SelectNode (lExtrs <> rExtrs) (lJoinTree <> rJoinTree)

data ObjectSelectSource
  = ObjectSelectSource
  { _ossPrefix :: !PG.Identifier
  , _ossFrom   :: !PG.FromItem
  , _ossWhere  :: !PG.BoolExp
  } deriving (Show, Eq, Generic)
instance Hashable ObjectSelectSource

objectSelectSourceToSelectSource :: ObjectSelectSource -> (SelectSource backend)
objectSelectSourceToSelectSource ObjectSelectSource{..} =
  SelectSource _ossPrefix _ossFrom Nothing _ossWhere Nothing Nothing Nothing

data ObjectRelationSource (b :: BackendType)
  = ObjectRelationSource
  { _orsRelationshipName :: !RelName
  , _orsRelationMapping  :: !(HM.HashMap (Column b) (Column b))
  , _orsSelectSource     :: !ObjectSelectSource
  } deriving (Generic)
instance Hashable (ObjectRelationSource 'Postgres)
deriving instance Eq (Column b) => Eq (ObjectRelationSource b)

data ArrayRelationSource (b :: BackendType)
  = ArrayRelationSource
  { _arsAlias           :: !(Alias b)
  , _arsRelationMapping :: !(HM.HashMap (Column b) (Column b))
  , _arsSelectSource    :: !(SelectSource b)
  } deriving (Generic)
instance Hashable (ArrayRelationSource 'Postgres)
deriving instance Eq (ArrayRelationSource 'Postgres)

data ArraySelectNode (b :: BackendType)
  = ArraySelectNode
  { _asnTopExtractors :: ![PG.Extractor]
  , _asnSelectNode    :: !(SelectNode b)
  }

instance Semigroup (ArraySelectNode 'Postgres) where
  ArraySelectNode lTopExtrs lSelNode <> ArraySelectNode rTopExtrs rSelNode =
    ArraySelectNode (lTopExtrs <> rTopExtrs) (lSelNode <> rSelNode)

data ComputedFieldTableSetSource (b :: BackendType)
  = ComputedFieldTableSetSource
  { _cftssFieldName    :: !FieldName
  , _cftssSelectType   :: !JsonAggSelect
  , _cftssSelectSource :: !(SelectSource b)
  } deriving (Generic)
instance Hashable (ComputedFieldTableSetSource 'Postgres)
deriving instance Show (ComputedFieldTableSetSource 'Postgres)
deriving instance Eq   (ComputedFieldTableSetSource 'Postgres)

data ArrayConnectionSource (b :: BackendType)
  = ArrayConnectionSource
  { _acsAlias           :: !(Alias b)
  , _acsRelationMapping :: !(HM.HashMap (Column b) (Column b))
  , _acsSplitFilter     :: !(Maybe PG.BoolExp)
  , _acsSlice           :: !(Maybe ConnectionSlice)
  , _acsSource          :: !(SelectSource b)
  } deriving (Generic)
deriving instance Eq (ArrayConnectionSource 'Postgres)

instance Hashable (ArrayConnectionSource 'Postgres)

data JoinTree (b :: BackendType)
  = JoinTree
  { _jtObjectRelations        :: !(HM.HashMap (ObjectRelationSource b) (SelectNode b))
  , _jtArrayRelations         :: !(HM.HashMap (ArrayRelationSource b) (ArraySelectNode b))
  , _jtArrayConnections       :: !(HM.HashMap (ArrayConnectionSource b) (ArraySelectNode b))
  , _jtComputedFieldTableSets :: !(HM.HashMap (ComputedFieldTableSetSource b) (SelectNode b))
  }

instance Semigroup (JoinTree 'Postgres) where
  JoinTree lObjs lArrs lArrConns lCfts <> JoinTree rObjs rArrs rArrConns rCfts =
    JoinTree (HM.unionWith (<>) lObjs rObjs)
             (HM.unionWith (<>) lArrs rArrs)
             (HM.unionWith (<>) lArrConns rArrConns)
             (HM.unionWith (<>) lCfts rCfts)

instance Monoid (JoinTree 'Postgres) where
  mempty = JoinTree mempty mempty mempty mempty


data PermissionLimitSubQuery
  = PLSQRequired !Int -- ^ Permission limit
  | PLSQNotRequired
  deriving (Show, Eq)
