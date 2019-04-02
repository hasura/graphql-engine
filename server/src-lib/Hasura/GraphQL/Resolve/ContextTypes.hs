module Hasura.GraphQL.Resolve.ContextTypes where

import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.Sequence                     as Seq
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.SQL.Types

data RelSelFld
  = RelSelFld
  { _rsfInfo       :: !RelInfo
  , _rsfPermFltr   :: !AnnBoolExpSQL
  , _rsfPermLimit  :: !(Maybe Int)
  , _rsfAllowAgg   :: !Bool
  , _rsfIsNullable :: !Bool
  } deriving (Show, Eq)

data SelFieldG a
  = SFldPGCol !PGColInfo
  | SFldCompCol !(Seq.Seq FuncArgItem) !CompColFunc
  | SFldRel !a
  deriving (Show, Eq)

type SelField = SelFieldG RelSelFld

data RelSelCtx
  = RelSelCtx
  { _rscInfo      :: !RelInfo
  , _rscPermFltr  :: !AnnBoolExpSQL
  , _rscPermLimit :: !(Maybe Int)
  , _rscIsAgg     :: !Bool
  } deriving (Show, Eq)

type SelFieldCtx = SelFieldG RelSelCtx

getColsFromSelFields :: [SelFieldG a] -> [PGColInfo]
getColsFromSelFields flds =
  flip mapMaybe flds $ \case
    SFldPGCol ci -> Just ci
    _            -> Nothing

getRelsFromSelFields :: [SelField] -> [RelSelFld]
getRelsFromSelFields flds =
  flip mapMaybe flds $ \case
    SFldRel rcs  -> Just rcs
    _            -> Nothing

getCCFuncsFromSelFields :: [SelFieldG a] -> [CompColFunc]
getCCFuncsFromSelFields flds =
  flip mapMaybe flds $ \case
    SFldCompCol _ ccf -> Just ccf
    _                 -> Nothing

type FieldMap
  = Map.HashMap (G.NamedType, G.Name) SelFieldCtx

-- order by context
data OrdByItem
  = OBIPGCol !PGColInfo
  | OBIRel !RelInfo !AnnBoolExpSQL
  | OBIAgg !RelInfo !AnnBoolExpSQL
  deriving (Show, Eq)

type OrdByItemMap = Map.HashMap G.Name OrdByItem

type OrdByCtx = Map.HashMap G.NamedType OrdByItemMap

newtype FuncArgItem
  = FuncArgItem {getArgName :: G.Name}
  deriving (Show, Eq)

type FuncArgSeq = Seq.Seq FuncArgItem

-- insert context
type RelationInfoMap = Map.HashMap RelName RelInfo

data UpdPermForIns
  = UpdPermForIns
  { upfiCols   :: ![PGCol]
  , upfiFilter :: !AnnBoolExpSQL
  , upfiSet    :: !PreSetCols
  } deriving (Show, Eq)

data InsCtx
  = InsCtx
  { icView      :: !QualifiedTable
  , icColumns   :: ![PGColInfo]
  , icSet       :: !PreSetCols
  , icRelations :: !RelationInfoMap
  , icUpdPerm   :: !(Maybe UpdPermForIns)
  } deriving (Show, Eq)

type InsCtxMap = Map.HashMap QualifiedTable InsCtx

type PGColArgMap = Map.HashMap G.Name PGColInfo
