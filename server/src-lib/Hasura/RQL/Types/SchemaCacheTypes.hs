module Hasura.RQL.Types.SchemaCacheTypes where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Hasura.Prelude

import qualified Data.HashMap.Strict         as M
import qualified Data.Text                   as T

import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.Subscribe
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML              as S

data TableObjId
  = TOCol !PGCol
  | TORel !RelName
  | TOCons !ConstraintName
  | TOPerm !RoleName !PermType
  | TOTrigger !TriggerName
  deriving (Show, Eq, Generic)

instance Hashable TableObjId

data SchemaObjId
  = SOTable !QualifiedTable
  | SOQTemplate !TQueryName
  | SOTableObj !QualifiedTable !TableObjId
   deriving (Eq, Generic)

instance Hashable SchemaObjId

reportSchemaObj :: SchemaObjId -> T.Text
reportSchemaObj (SOTable tn) = "table " <> qualTableToTxt tn
reportSchemaObj (SOQTemplate qtn) =
  "query-template " <> getTQueryName qtn
reportSchemaObj (SOTableObj tn (TOCol cn)) =
  "column " <> qualTableToTxt tn <> "." <> getPGColTxt cn
reportSchemaObj (SOTableObj tn (TORel cn)) =
  "relationship " <> qualTableToTxt tn <> "." <> getRelTxt cn
reportSchemaObj (SOTableObj tn (TOCons cn)) =
  "constraint " <> qualTableToTxt tn <> "." <> getConstraintTxt cn
reportSchemaObj (SOTableObj tn (TOPerm rn pt)) =
  "permission " <> qualTableToTxt tn <> "." <> getRoleTxt rn
  <> "." <> permTypeToCode pt
reportSchemaObj (SOTableObj tn (TOTrigger trn )) =
  "event-trigger " <> qualTableToTxt tn <> "." <> trn


instance Show SchemaObjId where
  show soi = T.unpack $ reportSchemaObj soi

instance ToJSON SchemaObjId where
  toJSON = String . reportSchemaObj

-- data PGColInfo
--   = PGColInfo
--   { pgiName       :: !PGCol
--   , pgiType       :: !PGColType
--   , pgiIsNullable :: !Bool
--   } deriving (Show, Eq)

-- $(deriveToJSON (aesonDrop 3 snakeCase) ''PGColInfo)

instance ToJSONKey SchemaObjId where
  toJSONKey = toJSONKeyText reportSchemaObj

data SchemaDependency
  = SchemaDependency
  { sdObjId  :: !SchemaObjId
  , sdReason :: !T.Text
  } deriving (Show, Eq, Generic)

$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaDependency)
instance Hashable SchemaDependency

-- data RelInfo
--   = RelInfo
--   { riName     :: !RelName
--   , riType     :: !RelType
--   , riMapping  :: ![(PGCol, PGCol)]
--   , riRTable   :: !QualifiedTable
--   , riDeps     :: ![SchemaDependency]
--   , riIsManual :: !Bool
--   } deriving (Show, Eq)

-- $(deriveToJSON (aesonDrop 2 snakeCase) ''RelInfo)

type InsSetCols = M.HashMap PGCol S.SQLExp
