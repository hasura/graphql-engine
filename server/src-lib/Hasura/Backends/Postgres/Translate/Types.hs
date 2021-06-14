{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.Postgres.Translate.Types where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                as HM

import           Data.Int                           (Int64)

import qualified Hasura.Backends.Postgres.SQL.DML   as PG
import qualified Hasura.Backends.Postgres.SQL.Types as PG

import           Hasura.RQL.IR.Select
import           Hasura.RQL.Types.Common


data SourcePrefixes
  = SourcePrefixes
  { _pfThis :: !PG.Identifier -- ^ Current source prefix
  , _pfBase :: !PG.Identifier
  -- ^ Base table source row identifier to generate
  -- the table's column identifiers for computed field
  -- function input parameters
  } deriving (Show, Eq, Generic)
instance Hashable SourcePrefixes

data SelectSource
  = SelectSource
  { _ssPrefix   :: !PG.Identifier
  , _ssFrom     :: !PG.FromItem
  , _ssDistinct :: !(Maybe PG.DistinctExpr)
  , _ssWhere    :: !PG.BoolExp
  , _ssOrderBy  :: !(Maybe PG.OrderByExp)
  , _ssLimit    :: !(Maybe Int)
  , _ssOffset   :: !(Maybe Int64)
  } deriving (Generic)
instance Hashable SelectSource
deriving instance Show SelectSource
deriving instance Eq   SelectSource

data SelectNode
  = SelectNode
  { _snExtractors :: !(HM.HashMap PG.Alias PG.SQLExp)
  , _snJoinTree   :: !JoinTree
  }

instance Semigroup SelectNode where
  SelectNode lExtrs lJoinTree <> SelectNode rExtrs rJoinTree =
    SelectNode (lExtrs <> rExtrs) (lJoinTree <> rJoinTree)

data ObjectSelectSource
  = ObjectSelectSource
  { _ossPrefix :: !PG.Identifier
  , _ossFrom   :: !PG.FromItem
  , _ossWhere  :: !PG.BoolExp
  } deriving (Show, Eq, Generic)
instance Hashable ObjectSelectSource

objectSelectSourceToSelectSource :: ObjectSelectSource -> SelectSource
objectSelectSourceToSelectSource ObjectSelectSource{..} =
  SelectSource _ossPrefix _ossFrom Nothing _ossWhere Nothing Nothing Nothing

data ObjectRelationSource
  = ObjectRelationSource
  { _orsRelationshipName :: !RelName
  , _orsRelationMapping  :: !(HM.HashMap PG.PGCol PG.PGCol)
  , _orsSelectSource     :: !ObjectSelectSource
  } deriving (Generic)
instance Hashable ObjectRelationSource
deriving instance Eq ObjectRelationSource

data ArrayRelationSource
  = ArrayRelationSource
  { _arsAlias           :: !PG.Alias
  , _arsRelationMapping :: !(HM.HashMap PG.PGCol PG.PGCol)
  , _arsSelectSource    :: !SelectSource
  } deriving (Generic)
instance Hashable ArrayRelationSource
deriving instance Eq ArrayRelationSource

data ArraySelectNode
  = ArraySelectNode
  { _asnTopExtractors :: ![PG.Extractor]
  , _asnSelectNode    :: !SelectNode
  }

instance Semigroup ArraySelectNode where
  ArraySelectNode lTopExtrs lSelNode <> ArraySelectNode rTopExtrs rSelNode =
    ArraySelectNode (lTopExtrs <> rTopExtrs) (lSelNode <> rSelNode)

data ComputedFieldTableSetSource
  = ComputedFieldTableSetSource
  { _cftssFieldName    :: !FieldName
  , _cftssSelectType   :: !JsonAggSelect
  , _cftssSelectSource :: !SelectSource
  } deriving (Generic)
instance Hashable ComputedFieldTableSetSource
deriving instance Show ComputedFieldTableSetSource
deriving instance Eq   ComputedFieldTableSetSource

data ArrayConnectionSource
  = ArrayConnectionSource
  { _acsAlias           :: !PG.Alias
  , _acsRelationMapping :: !(HM.HashMap PG.PGCol PG.PGCol)
  , _acsSplitFilter     :: !(Maybe PG.BoolExp)
  , _acsSlice           :: !(Maybe ConnectionSlice)
  , _acsSource          :: !SelectSource
  } deriving (Generic)
deriving instance Eq ArrayConnectionSource

instance Hashable ArrayConnectionSource

data JoinTree
  = JoinTree
  { _jtObjectRelations        :: !(HM.HashMap ObjectRelationSource SelectNode)
  , _jtArrayRelations         :: !(HM.HashMap ArrayRelationSource ArraySelectNode)
  , _jtArrayConnections       :: !(HM.HashMap ArrayConnectionSource ArraySelectNode)
  , _jtComputedFieldTableSets :: !(HM.HashMap ComputedFieldTableSetSource SelectNode)
  }

instance Semigroup JoinTree where
  JoinTree lObjs lArrs lArrConns lCfts <> JoinTree rObjs rArrs rArrConns rCfts =
    JoinTree (HM.unionWith (<>) lObjs rObjs)
             (HM.unionWith (<>) lArrs rArrs)
             (HM.unionWith (<>) lArrConns rArrConns)
             (HM.unionWith (<>) lCfts rCfts)

instance Monoid JoinTree where
  mempty = JoinTree mempty mempty mempty mempty


data PermissionLimitSubQuery
  = PLSQRequired !Int -- ^ Permission limit
  | PLSQNotRequired
  deriving (Show, Eq)
