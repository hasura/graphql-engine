module Hasura.GraphQL.Resolve.ContextTypes where

import           Hasura.Prelude

import qualified Data.HashMap.Strict               as Map
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
  deriving (Show, Eq)

type OrdByItemMap = Map.HashMap G.Name OrdByItem

type OrdByCtx = Map.HashMap G.NamedType OrdByItemMap

-- insert context
type RelationInfoMap = Map.HashMap RelName RelInfo

data InsCtx
  = InsCtx
  { icView      :: !QualifiedTable
  , icColumns   :: ![PGColInfo]
  , icSet       :: !InsSetCols
  , icRelations :: !RelationInfoMap
  } deriving (Show, Eq)

type InsCtxMap = Map.HashMap QualifiedTable InsCtx
