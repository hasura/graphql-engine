module Hasura.RQL.Types.SchemaCacheTypes where

import           Hasura.Prelude

import qualified Data.Text                           as T

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Hashable
import           Data.Text.Extended
import           Data.Text.NonEmpty

import qualified Hasura.SQL.AnyBackend               as AB

import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.ComputedField
import           Hasura.RQL.Types.EventTrigger
import           Hasura.RQL.Types.Instances          ()
import           Hasura.RQL.Types.Permission
import           Hasura.RQL.Types.RemoteRelationship
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.SQL.Backend
import           Hasura.Session


data TableObjId (b :: BackendType)
  = TOCol !(Column b)
  | TORel !RelName
  | TOComputedField !ComputedFieldName
  | TORemoteRel !RemoteRelationshipName
  | TOForeignKey !(ConstraintName b)
  | TOPerm !RoleName !PermType
  | TOTrigger !TriggerName
  deriving (Generic)
instance (Backend b) => Hashable (TableObjId b)
deriving instance Backend b => Eq (TableObjId b)
deriving instance Backend b => Show (TableObjId b)

data SourceObjId (b :: BackendType)
  = SOITable !(TableName b)
  | SOITableObj !(TableName b) !(TableObjId b)
  | SOIFunction !(FunctionName b)
  deriving (Show, Eq, Generic)
instance (Backend b) => Hashable (SourceObjId b)

data SchemaObjId
  = SOSource !SourceName
  | SOSourceObj !SourceName !(AB.AnyBackend SourceObjId)
  | SORemoteSchema !RemoteSchemaName
  | SORemoteSchemaPermission !RemoteSchemaName !RoleName
  | SORole !RoleName

instance Hashable SchemaObjId where
  hashWithSalt salt = \case
    SOSource sourceName -> hashWithSalt salt sourceName
    SOSourceObj sourceName exists ->
      AB.dispatchAnyBackend' @Hashable exists (hashWithSalt salt . (sourceName,))
    SORemoteSchema remoteSchemaName -> hashWithSalt salt remoteSchemaName
    SORemoteSchemaPermission remoteSchemaName roleName -> hashWithSalt salt (remoteSchemaName, roleName)
    SORole roleName -> hashWithSalt salt roleName

instance Eq SchemaObjId where
  (SOSource s1) == (SOSource s2)                                       = s1 == s2
  (SORemoteSchema s1) == (SORemoteSchema s2)                           = s1 == s2
  (SOSourceObj s1 id1) == (SOSourceObj s2 id2)                         = (s1 == s2) && id1 == id2
  (SORemoteSchemaPermission s1 r1) == (SORemoteSchemaPermission s2 r2) = (s1, r1) == (s2, r2)
  (SORole r1) == (SORole r2)                                           = r1 == r2
  _ == _                                                               = False

reportSchemaObj :: SchemaObjId -> T.Text
reportSchemaObj = \case
  SOSource source -> "source " <> sourceNameToText source
  SOSourceObj source exists -> inSource source $
    AB.dispatchAnyBackend @Backend exists
      \case
        SOITable tn -> "table " <> toTxt tn
        SOIFunction fn -> "function " <> toTxt fn
        SOITableObj tn (TOCol cn) ->
          "column " <> toTxt tn <> "." <> toTxt cn
        SOITableObj tn (TORel cn) ->
          "relationship " <> toTxt tn <> "." <> toTxt cn
        SOITableObj tn (TOForeignKey cn) ->
          "constraint " <> toTxt tn <> "." <> toTxt cn
        SOITableObj tn (TOPerm rn pt) ->
          "permission " <> toTxt tn <> "." <> roleNameToTxt rn <> "." <> permTypeToCode pt
        SOITableObj tn (TOTrigger trn ) ->
          "event-trigger " <> toTxt tn <> "." <> triggerNameToTxt trn
        SOITableObj tn (TOComputedField ccn) ->
          "computed field " <> toTxt tn <> "." <> computedFieldNameToText ccn
        SOITableObj tn (TORemoteRel rn) ->
          "remote relationship " <> toTxt tn <> "." <> remoteRelationshipNameToText rn
  SORemoteSchema remoteSchemaName ->
    "remote schema " <> unNonEmptyText (unRemoteSchemaName remoteSchemaName)
  SORemoteSchemaPermission remoteSchemaName roleName ->
    "remote schema permission "
    <> unNonEmptyText (unRemoteSchemaName remoteSchemaName)
    <> "." <>> roleName
  SORole roleName -> "role " <> roleNameToTxt roleName
  where
    inSource s t = t <> " in source " <>> s

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
  | DRParentRole
  deriving (Show, Eq, Generic)

instance Hashable DependencyReason

reasonToTxt :: DependencyReason -> Text
reasonToTxt = \case
  DRTable              -> "table"
  DRColumn             -> "column"
  DRRemoteTable        -> "remote_table"
  DRLeftColumn         -> "left_column"
  DRRightColumn        -> "right_column"
  DRUsingColumn        -> "using_column"
  DRFkey               -> "fkey"
  DRRemoteFkey         -> "remote_fkey"
  DRUntyped            -> "untyped"
  DROnType             -> "on_type"
  DRSessionVariable    -> "session_variable"
  DRPayload            -> "payload"
  DRParent             -> "parent"
  DRRemoteSchema       -> "remote_schema"
  DRRemoteRelationship -> "remote_relationship"
  DRParentRole         -> "parent_role"

instance ToJSON DependencyReason where
  toJSON = String . reasonToTxt

data SchemaDependency
  = SchemaDependency
  { sdObjId  :: !SchemaObjId
  , sdReason :: !DependencyReason
  } deriving (Show, Eq, Generic)

$(deriveToJSON hasuraJSON ''SchemaDependency)
instance Hashable SchemaDependency
