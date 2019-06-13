module Hasura.GraphQL.Resolve.ContextTypes where

import           Control.Lens.TH
import           Hasura.Prelude

import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Seq
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Common
import           Hasura.RQL.DDL.Remote.Types
import           Hasura.SQL.Types

data TypedField
  = FldCol PGColInfo
  | FldRel (RelInfo, Bool, AnnBoolExpPartialSQL, Maybe Int)
  | FldRemote RemoteField
  deriving (Show, Eq)

$(makePrisms ''TypedField)

type FieldMap
  = Map.HashMap (G.NamedType, G.Name)
    TypedField

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
