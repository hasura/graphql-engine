module Hasura.GraphQL.Resolve.Types where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML                as S

type OpCtxMap = Map.HashMap G.Name OpCtx

data InsOpCtx
  = InsOpCtx
  { _iocTable   :: !QualifiedTable
  , _iocHeaders :: ![T.Text]
  } deriving (Show, Eq)

data SelOpCtx
  = SelOpCtx
  { _socTable   :: !QualifiedTable
  , _socHeaders :: ![T.Text]
  , _socFilter  :: !AnnBoolExpPartialSQL
  , _socLimit   :: !(Maybe Int)
  } deriving (Show, Eq)

data SelPkOpCtx
  = SelPkOpCtx
  { _spocTable   :: !QualifiedTable
  , _spocHeaders :: ![T.Text]
  , _spocFilter  :: !AnnBoolExpPartialSQL
  , _spocArgMap  :: !PGColArgMap
  } deriving (Show, Eq)

data FuncQOpCtx
  = FuncQOpCtx
  { _fqocTable    :: !QualifiedTable
  , _fqocHeaders  :: ![T.Text]
  , _fqocFilter   :: !AnnBoolExpPartialSQL
  , _fqocLimit    :: !(Maybe Int)
  , _fqocFunction :: !QualifiedFunction
  , _fqocArgs     :: !FuncArgSeq
  } deriving (Show, Eq)

data UpdOpCtx
  = UpdOpCtx
  { _uocTable      :: !QualifiedTable
  , _uocHeaders    :: ![T.Text]
  , _uocFilter     :: !AnnBoolExpPartialSQL
  , _uocPresetCols :: !PreSetColsPartial
  , _uocAllCols    :: ![PGColInfo]
  } deriving (Show, Eq)

data DelOpCtx
  = DelOpCtx
  { _docTable   :: !QualifiedTable
  , _docHeaders :: ![T.Text]
  , _docFilter  :: !AnnBoolExpPartialSQL
  , _docAllCols :: ![PGColInfo]
  } deriving (Show, Eq)

data OpCtx
  = OCSelect !SelOpCtx
  | OCSelectPkey !SelPkOpCtx
  | OCSelectAgg !SelOpCtx
  | OCFuncQuery !FuncQOpCtx
  | OCFuncAggQuery !FuncQOpCtx
  | OCInsert !InsOpCtx
  | OCUpdate !UpdOpCtx
  | OCDelete !DelOpCtx
  deriving (Show, Eq)

type FieldMap
  = Map.HashMap (G.NamedType, G.Name)
    (Either PGColInfo (RelInfo, Bool, AnnBoolExpPartialSQL, Maybe Int))

-- order by context
data OrdByItem
  = OBIPGCol !PGColInfo
  | OBIRel !RelInfo !AnnBoolExpPartialSQL
  | OBIAgg !RelInfo !AnnBoolExpPartialSQL
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
  , upfiFilter :: !AnnBoolExpPartialSQL
  , upfiSet    :: !PreSetColsPartial
  } deriving (Show, Eq)

data InsCtx
  = InsCtx
  { icView      :: !QualifiedTable
  , icAllCols   :: ![PGColInfo]
  , icSet       :: !PreSetColsPartial
  , icRelations :: !RelationInfoMap
  , icUpdPerm   :: !(Maybe UpdPermForIns)
  } deriving (Show, Eq)

type InsCtxMap = Map.HashMap QualifiedTable InsCtx

type PGColArgMap = Map.HashMap G.Name PGColInfo

data AnnPGVal
  = AnnPGVal
  { _apvVariable   :: !(Maybe G.Variable)
  , _apvIsNullable :: !Bool
  , _apvType       :: !PGColType
  , _apvValue      :: !PGColValue
  } deriving (Show, Eq)

type PrepFn m = AnnPGVal -> m S.SQLExp

-- lifts PartialSQLExp to UnresolvedVal
partialSQLExpToUnresolvedVal :: PartialSQLExp -> UnresolvedVal
partialSQLExpToUnresolvedVal = \case
  PSESessVar ty sessVar -> UVSessVar ty sessVar
  PSESQLExp s           -> UVSQL s

-- A value that will be converted to an sql expression eventually
data UnresolvedVal
  -- From a session variable
  = UVSessVar !PgType !SessVar
  -- This is postgres
  | UVPG !AnnPGVal
  -- This is a full resolved sql expression
  | UVSQL !S.SQLExp
  deriving (Show, Eq)

type AnnBoolExpUnresolved = AnnBoolExp UnresolvedVal
