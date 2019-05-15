module Hasura.RQL.Types.SchemaCacheTypes where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Hasura.Prelude

import qualified Data.Text                     as T

import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types

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
  | SOFunction !QualifiedFunction
   deriving (Eq, Generic)

instance Hashable SchemaObjId

reportSchemaObj :: SchemaObjId -> T.Text
reportSchemaObj (SOTable tn) = "table " <> qualObjectToText tn
reportSchemaObj (SOFunction fn) = "function " <> qualObjectToText fn
reportSchemaObj (SOQTemplate qtn) =
  "query-template " <> getTQueryName qtn
reportSchemaObj (SOTableObj tn (TOCol cn)) =
  "column " <> qualObjectToText tn <> "." <> getPGColTxt cn
reportSchemaObj (SOTableObj tn (TORel cn)) =
  "relationship " <> qualObjectToText tn <> "." <> getRelTxt cn
reportSchemaObj (SOTableObj tn (TOCons cn)) =
  "constraint " <> qualObjectToText tn <> "." <> getConstraintTxt cn
reportSchemaObj (SOTableObj tn (TOPerm rn pt)) =
  "permission " <> qualObjectToText tn <> "." <> getRoleTxt rn
  <> "." <> permTypeToCode pt
reportSchemaObj (SOTableObj tn (TOTrigger trn )) =
  "event-trigger " <> qualObjectToText tn <> "." <> trn

instance Show SchemaObjId where
  show soi = T.unpack $ reportSchemaObj soi

instance ToJSON SchemaObjId where
  toJSON = String . reportSchemaObj

instance ToJSONKey SchemaObjId where
  toJSONKey = toJSONKeyText reportSchemaObj

data SchemaDependency
  = SchemaDependency
  { sdObjId  :: !SchemaObjId
  , sdReason :: !T.Text
  } deriving (Show, Eq, Generic)

$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaDependency)
instance Hashable SchemaDependency
