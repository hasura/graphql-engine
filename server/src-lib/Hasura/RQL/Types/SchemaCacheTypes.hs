module Hasura.RQL.Types.SchemaCacheTypes where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Hasura.Prelude

import qualified Data.Text                           as T

import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.Session
import           Hasura.SQL.Types

data TableObjId
  = TOCol !PGCol
  | TORel !RelName
  | TOComputedField !ComputedFieldName
  | TORemoteRel !RemoteRelationshipName
  | TOForeignKey !ConstraintName
  | TOPerm !RoleName !PermType
  | TOTrigger !TriggerName
  deriving (Show, Eq, Generic)
instance Hashable TableObjId

data SchemaObjId
  = SOTable !QualifiedTable
  | SOTableObj !QualifiedTable !TableObjId
  | SOFunction !QualifiedFunction
  | SORemoteSchema !RemoteSchemaName
   deriving (Eq, Generic)

instance Hashable SchemaObjId

reportSchemaObj :: SchemaObjId -> T.Text
reportSchemaObj (SOTable tn) = "table " <> qualObjectToText tn
reportSchemaObj (SOFunction fn) = "function " <> qualObjectToText fn
reportSchemaObj (SOTableObj tn (TOCol cn)) =
  "column " <> qualObjectToText tn <> "." <> getPGColTxt cn
reportSchemaObj (SOTableObj tn (TORel cn)) =
  "relationship " <> qualObjectToText tn <> "." <> relNameToTxt cn
reportSchemaObj (SOTableObj tn (TOForeignKey cn)) =
  "constraint " <> qualObjectToText tn <> "." <> getConstraintTxt cn
reportSchemaObj (SOTableObj tn (TOPerm rn pt)) =
  "permission " <> qualObjectToText tn <> "." <> roleNameToTxt rn
  <> "." <> permTypeToCode pt
reportSchemaObj (SOTableObj tn (TOTrigger trn )) =
  "event-trigger " <> qualObjectToText tn <> "." <> triggerNameToTxt trn
reportSchemaObj (SOTableObj tn (TOComputedField ccn)) =
  "computed field " <> qualObjectToText tn <> "." <> computedFieldNameToText ccn
reportSchemaObj (SOTableObj tn (TORemoteRel rn)) =
  "remote relationship " <> qualObjectToText tn <> "." <> remoteRelationshipNameToText rn
reportSchemaObj (SORemoteSchema remoteSchemaName) =
  "remote schema " <> unNonEmptyText (unRemoteSchemaName remoteSchemaName)

instance Show SchemaObjId where
  show soi = T.unpack $ reportSchemaObj soi

instance ToJSON SchemaObjId where
  toJSON = String . reportSchemaObj

instance ToJSONKey SchemaObjId where
  toJSONKey = toJSONKeyText reportSchemaObj

data DependencyReason
  = DRTable
  | DRColumn
  | DRRemoteTable
  | DRLeftColumn
  | DRRightColumn
  | DRUsingColumn
  | DRFkey
  | DRRemoteFkey
  | DRUntyped
  | DROnType
  | DRSessionVariable
  | DRPayload
  | DRParent
  | DRRemoteSchema
  | DRRemoteRelationship
  deriving (Show, Eq, Generic)

instance Hashable DependencyReason

reasonToTxt :: DependencyReason -> Text
reasonToTxt = \case
  DRTable           -> "table"
  DRColumn          -> "column"
  DRRemoteTable     -> "remote_table"
  DRLeftColumn      -> "left_column"
  DRRightColumn     -> "right_column"
  DRUsingColumn     -> "using_column"
  DRFkey            -> "fkey"
  DRRemoteFkey      -> "remote_fkey"
  DRUntyped         -> "untyped"
  DROnType          -> "on_type"
  DRSessionVariable -> "session_variable"
  DRPayload         -> "payload"
  DRParent          -> "parent"
  DRRemoteSchema    -> "remote_schema"
  DRRemoteRelationship    -> "remote_relationship"

instance ToJSON DependencyReason where
  toJSON = String . reasonToTxt

data SchemaDependency
  = SchemaDependency
  { sdObjId  :: !SchemaObjId
  , sdReason :: !DependencyReason
  } deriving (Show, Eq, Generic)

$(deriveToJSON (aesonDrop 2 snakeCase) ''SchemaDependency)
instance Hashable SchemaDependency
