module Hasura.GraphQL.Resolve.Types
  ( module Hasura.GraphQL.Resolve.Types
  -- * Re-exports
  , MonadReusability(..)
  ) where

import           Control.Lens.TH
import           Hasura.Prelude

import qualified Data.HashMap.Strict            as Map
import qualified Data.Sequence                  as Seq
import qualified Data.Text                      as T
import qualified Language.GraphQL.Draft.Syntax  as G

import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.Function
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML                 as S

data QueryCtx
  = QCSelect !SelOpCtx
  | QCSelectPkey !SelPkOpCtx
  | QCSelectAgg !SelOpCtx
  | QCFuncQuery !FuncQOpCtx
  | QCFuncAggQuery !FuncQOpCtx
  deriving (Show, Eq)

data MutationCtx
  = MCInsert !InsOpCtx
  | MCUpdate !UpdOpCtx
  | MCDelete !DelOpCtx
  deriving (Show, Eq)

type OpCtxMap a = Map.HashMap G.Name a
type QueryCtxMap = OpCtxMap QueryCtx
type MutationCtxMap = OpCtxMap MutationCtx

data InsOpCtx
  = InsOpCtx
  { _iocTable   :: !QualifiedTable
  , _iocHeaders :: ![T.Text]
  } deriving (Show, Eq)

data SelOpCtx
  = SelOpCtx
  { _socTable   :: !QualifiedTable
  , _socHeaders :: ![T.Text]
  , _socAllCols :: !PGColGNameMap
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

type FunctionArgSeq = Seq.Seq (InputArgument FunctionArgItem)

data FuncQOpCtx
  = FuncQOpCtx
  { _fqocFunction :: !QualifiedFunction
  , _fqocArgs     :: !FunctionArgSeq
  , _fqocHeaders  :: ![T.Text]
  , _fqocAllCols  :: !PGColGNameMap
  , _fqocFilter   :: !AnnBoolExpPartialSQL
  , _fqocLimit    :: !(Maybe Int)
  } deriving (Show, Eq)

data UpdOpCtx
  = UpdOpCtx
  { _uocTable      :: !QualifiedTable
  , _uocHeaders    :: ![T.Text]
  , _uocAllCols    :: !PGColGNameMap
  , _uocFilter     :: !AnnBoolExpPartialSQL
  , _uocPresetCols :: !PreSetColsPartial
  } deriving (Show, Eq)

data DelOpCtx
  = DelOpCtx
  { _docTable   :: !QualifiedTable
  , _docHeaders :: ![T.Text]
  , _docFilter  :: !AnnBoolExpPartialSQL
  , _docAllCols :: ![PGColumnInfo]
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

-- (custom name | generated name) -> PG column info
-- used in resolvers
type PGColGNameMap = Map.HashMap G.Name PGColumnInfo

data RelationshipField
  = RelationshipField
  { _rfInfo       :: !RelInfo
  , _rfIsAgg      :: !Bool
  , _rfCols       :: !PGColGNameMap
  , _rfPermFilter :: !AnnBoolExpPartialSQL
  , _rfPermLimit  :: !(Maybe Int)
  } deriving (Show, Eq)

data ComputedFieldTable
  = ComputedFieldTable
  { _cftTable      :: !QualifiedTable
  , _cftCols       :: !PGColGNameMap
  , _cftPermFilter :: !AnnBoolExpPartialSQL
  , _cftPermLimit  :: !(Maybe Int)
  } deriving (Show, Eq)

data ComputedFieldType
  = CFTScalar !PGScalarType
  | CFTTable !ComputedFieldTable
  deriving (Show, Eq)

type ComputedFieldFunctionArgSeq = Seq.Seq FunctionArgItem

data ComputedField
  = ComputedField
  { _cfName     :: !ComputedFieldName
  , _cfFunction :: !ComputedFieldFunction
  , _cfArgSeq   :: !ComputedFieldFunctionArgSeq
  , _cfType     :: !ComputedFieldType
  } deriving (Show, Eq)

data ResolveField
  = RFPGColumn !PGColumnInfo
  | RFRelationship !RelationshipField
  | RFComputedField !ComputedField
  deriving (Show, Eq)

type FieldMap = Map.HashMap (G.NamedType, G.Name) ResolveField

-- order by context
data OrdByItem
  = OBIPGCol !PGColumnInfo
  | OBIRel !RelInfo !AnnBoolExpPartialSQL
  | OBIAgg !RelInfo !PGColGNameMap !AnnBoolExpPartialSQL
  deriving (Show, Eq)

type OrdByItemMap = Map.HashMap G.Name OrdByItem

type OrdByCtx = Map.HashMap G.NamedType OrdByItemMap

data FunctionArgItem
  = FunctionArgItem
  { _faiInputArgName :: !G.Name
  , _faiSqlArgName   :: !(Maybe FunctionArgName)
  , _faiHasDefault   :: !HasDefault
  } deriving (Show, Eq)

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
  , icAllCols   :: !PGColGNameMap
  , icSet       :: !PreSetColsPartial
  , icRelations :: !RelationInfoMap
  , icUpdPerm   :: !(Maybe UpdPermForIns)
  } deriving (Show, Eq)

type InsCtxMap = Map.HashMap QualifiedTable InsCtx

type PGColArgMap = Map.HashMap G.Name PGColumnInfo

data AnnPGVal
  = AnnPGVal
  { _apvVariable   :: !(Maybe G.Variable)
  , _apvIsNullable :: !Bool
  , _apvValue      :: !(WithScalarType PGScalarValue)
  } deriving (Show, Eq)

type PrepFn m = AnnPGVal -> m S.SQLExp

-- lifts PartialSQLExp to UnresolvedVal
partialSQLExpToUnresolvedVal :: PartialSQLExp -> UnresolvedVal
partialSQLExpToUnresolvedVal = \case
  PSESessVar ty sessVar -> UVSessVar ty sessVar
  PSESQLExp s           -> UVSQL s

-- | A value that will be converted to an sql expression eventually
data UnresolvedVal
  -- | an entire session variables JSON object
  = UVSession
  | UVSessVar !(PGType PGScalarType) !SessVar
  -- | a SQL value literal that can be parameterized over
  | UVPG !AnnPGVal
  -- | an arbitrary SQL expression, which /cannot/ be parameterized over
  | UVSQL !S.SQLExp
  deriving (Show, Eq)

type AnnBoolExpUnresolved = AnnBoolExp UnresolvedVal

-- template haskell related
$(makePrisms ''ResolveField)

data InputFunctionArgument
  = IFAKnown !FunctionArgName !UnresolvedVal -- ^ Known value
  | IFAUnknown !FunctionArgItem -- ^ Unknown value, need to be parsed
  deriving (Show, Eq)
$(makePrisms ''InputFunctionArgument)
