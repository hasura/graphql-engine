{-# LANGUAGE UndecidableInstances #-}

module Hasura.GraphQL.Resolve.Types where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML                as S

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

data FuncQOpCtx
  = FuncQOpCtx
  { _fqocTable    :: !QualifiedTable
  , _fqocHeaders  :: ![T.Text]
  , _fqocAllCols  :: !PGColGNameMap
  , _fqocFilter   :: !AnnBoolExpPartialSQL
  , _fqocLimit    :: !(Maybe Int)
  , _fqocFunction :: !QualifiedFunction
  , _fqocArgs     :: !FuncArgSeq
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

type FieldMap =
  Map.HashMap (G.NamedType, G.Name)
  (Either PGColumnInfo RelationshipField)

-- order by context
data OrdByItem
  = OBIPGCol !PGColumnInfo
  | OBIRel !RelInfo !AnnBoolExpPartialSQL
  | OBIAgg !RelInfo !PGColGNameMap !AnnBoolExpPartialSQL
  deriving (Show, Eq)

type OrdByItemMap = Map.HashMap G.Name OrdByItem

type OrdByCtx = Map.HashMap G.NamedType OrdByItemMap

data FuncArgItem
  = FuncArgItem
  { _faiInputArgName :: !G.Name
  , _faiSqlArgName   :: !(Maybe FunctionArgName)
  , _faiHasDefault   :: !Bool
  } deriving (Show, Eq)

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
  = UVSessVar !(PGType PGScalarType) !SessVar
  -- | a SQL value literal that can be parameterized over
  | UVPG !AnnPGVal
  -- | an arbitrary SQL expression, which /cannot/ be parameterized over
  | UVSQL !S.SQLExp
  deriving (Show, Eq)

type AnnBoolExpUnresolved = AnnBoolExp UnresolvedVal

-- | Tracks whether or not a query is /reusable/. Reusable queries are nice, since we can cache
-- their resolved ASTs and avoid re-resolving them if we receive an identical query. However, we
-- can’t always safely reuse queries if they have variables, since some variable values can affect
-- the generated SQL. For example, consider the following query:
--
-- > query users_where($condition: users_bool_exp!) {
-- >   users(where: $condition) {
-- >     id
-- >   }
-- > }
--
-- Different values for @$condition@ will produce completely different queries, so we can’t reuse
-- its plan (unless the variable values were also all identical, of course, but we don’t bother
-- caching those).
--
-- If a query does turn out to be reusable, we build up a 'ReusableVariableTypes' value that maps
-- variable names to their types so that we can use a fast path for validating new sets of
-- variables (namely 'Hasura.GraphQL.Validate.validateVariablesForReuse').
data QueryReusability
  = Reusable !ReusableVariableTypes
  | NotReusable
  deriving (Show, Eq)

instance Semigroup QueryReusability where
  Reusable a <> Reusable b = Reusable (a <> b)
  _          <> _          = NotReusable
instance Monoid QueryReusability where
  mempty = Reusable mempty

class (MonadError QErr m) => MonadResolve m where
  recordVariableUse :: G.Variable -> PGColumnType -> m ()
  markNotReusable :: m ()

newtype ResolveT m a = ResolveT { unResolveT :: StateT QueryReusability m a }
  deriving (Functor, Applicative, Monad, MonadError e, MonadReader r)

instance (MonadError QErr m) => MonadResolve (ResolveT m) where
  recordVariableUse varName varType = ResolveT $
    modify' (<> Reusable (ReusableVariableTypes $ Map.singleton varName varType))
  markNotReusable = ResolveT $ put NotReusable

runResolveT :: (Functor m) => ResolveT m a -> m (a, Maybe ReusableVariableTypes)
runResolveT = fmap (fmap getVarTypes) . flip runStateT mempty . unResolveT
  where
    getVarTypes = \case
      Reusable varTypes -> Just varTypes
      NotReusable       -> Nothing

evalResolveT :: (Monad m) => ResolveT m a -> m a
evalResolveT = flip evalStateT mempty . unResolveT
