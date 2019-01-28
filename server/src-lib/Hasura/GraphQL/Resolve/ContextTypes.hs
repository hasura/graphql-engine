module Hasura.GraphQL.Resolve.ContextTypes where

import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
import qualified Data.Sequence                     as Seq
import qualified Language.GraphQL.Draft.Syntax     as G

import           Hasura.RQL.Types.BoolExp
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.SchemaCacheTypes
import           Hasura.SQL.Types


type FieldMap
  = Map.HashMap (G.NamedType, G.Name)
    (Either PGColInfo (RelInfo, Bool, AnnBoolExpSQL, Maybe Int))

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

type FuncArgCtx = Map.HashMap G.NamedType (Seq.Seq FuncArgItem)

-- insert context
type RelationInfoMap = Map.HashMap RelName RelInfo

type UpdPermForIns = ([PGCol], AnnBoolExpSQL)

data InsCtx
  = InsCtx
  { icView      :: !QualifiedTable
  , icColumns   :: ![PGColInfo]
  , icSet       :: !InsSetCols
  , icRelations :: !RelationInfoMap
  , icUpdPerm   :: !(Maybe UpdPermForIns)
  } deriving (Show, Eq)

type InsCtxMap = Map.HashMap QualifiedTable InsCtx
