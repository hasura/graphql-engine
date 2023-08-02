{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.Types.SchemaCacheTypes
  ( BoolExpM (..),
    GetAggregationPredicatesDeps (..),
    BoolExpCtx (..),
    DependencyReason (..),
    SchemaDependency (..),
    SchemaObjId (..),
    SourceObjId (..),
    TableObjId (..),
    LogicalModelObjId (..),
    NativeQueryObjId (..),
    StoredProcedureObjId (..),
    purgeDependentObject,
    purgeSourceAndSchemaDependencies,
    reasonToTxt,
    reportDependentObjectsExist,
    reportSchemaObj,
    reportSchemaObjs,
    runBoolExpM,
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Const
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.NonEmpty
import Hasura.Base.Error
import Hasura.LogicalModel.Types (LogicalModelLocation, LogicalModelName)
import Hasura.NativeQuery.Types (NativeQueryName)
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp (PartialSQLExp)
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Instances ()
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Permission
import Hasura.RQL.Types.Roles (RoleName, roleNameToTxt)
import Hasura.RemoteSchema.Metadata
import Hasura.SQL.AnyBackend qualified as AB
import Language.GraphQL.Draft.Syntax qualified as G

data TableObjId (b :: BackendType)
  = TOCol (Column b)
  | TORel RelName
  | TOComputedField ComputedFieldName
  | TORemoteRel RelName
  | TOForeignKey (ConstraintName b)
  | TOPerm RoleName PermType
  | TOTrigger TriggerName
  deriving (Generic)

deriving instance (Backend b) => Eq (TableObjId b)

instance (Backend b) => Hashable (TableObjId b)

-- | Identifiers for components of logical models within the metadata. These
-- are used to track dependencies within the resolved schema (see
-- 'SourceInfo').
data LogicalModelObjId (b :: BackendType)
  = LMOPerm RoleName PermType
  | LMOCol (Column b)
  | LMOReferencedLogicalModel LogicalModelName
  deriving (Generic)

deriving stock instance (Backend b) => Eq (LogicalModelObjId b)

instance (Backend b) => Hashable (LogicalModelObjId b)

-- | Identifier for component of Native Queries within the metadata. These are
-- used to track dependencies between items in the resolved schema. For
-- instance, we use `NQOCol` along with `TOCol` from `TableObjId` to ensure
-- that the two columns that join an array relationship actually exist.
data NativeQueryObjId (b :: BackendType)
  = NQOCol (Column b)
  deriving (Generic)

deriving instance (Backend b) => Eq (NativeQueryObjId b)

instance (Backend b) => Hashable (NativeQueryObjId b)

-- | Identifier for component of Stored Procedures within the metadata. These are
-- used to track dependencies between items in the resolved schema. For
-- instance, we use `SPOCol` along with `TOCol` from `TableObjId` to ensure
-- that the two columns that join an array relationship actually exist.
newtype StoredProcedureObjId (b :: BackendType)
  = SPOCol (Column b)
  deriving (Generic)

deriving instance (Backend b) => Eq (StoredProcedureObjId b)

instance (Backend b) => Hashable (StoredProcedureObjId b)

data SourceObjId (b :: BackendType)
  = SOITable (TableName b)
  | SOITableObj (TableName b) (TableObjId b)
  | SOIFunction (FunctionName b)
  | SOINativeQuery NativeQueryName
  | SOINativeQueryObj NativeQueryName (NativeQueryObjId b)
  | SOIStoredProcedure (FunctionName b)
  | SOIStoredProcedureObj (FunctionName b) (StoredProcedureObjId b)
  | SOILogicalModel LogicalModelName
  | SOILogicalModelObj LogicalModelLocation (LogicalModelObjId b)
  deriving (Eq, Generic)

instance (Backend b) => Hashable (SourceObjId b)

data SchemaObjId
  = SOSource SourceName
  | SOSourceObj SourceName (AB.AnyBackend SourceObjId)
  | SORemoteSchema RemoteSchemaName
  | SORemoteSchemaPermission RemoteSchemaName RoleName
  | -- | A remote relationship on a remote schema type, identified by
    -- 1. remote schema name
    -- 2. remote schema type on which the relationship is defined
    -- 3. name of the relationship
    SORemoteSchemaRemoteRelationship RemoteSchemaName G.Name RelName
  | SORole RoleName
  deriving (Eq, Generic)

instance Hashable SchemaObjId

reportSchemaObj :: SchemaObjId -> T.Text
reportSchemaObj = \case
  SOSource source -> "source " <> sourceNameToText source
  SOSourceObj source exists -> inSource source
    $ AB.dispatchAnyBackend @Backend
      exists
      \case
        SOITable tn -> "table " <> toTxt tn
        SOIFunction fn -> "function " <> toTxt fn
        SOINativeQuery nqn -> "native query " <> toTxt nqn
        SOINativeQueryObj nqn (NQOCol cn) ->
          "column " <> toTxt nqn <> "." <> toTxt cn
        SOIStoredProcedure spn -> "stored procedure " <> toTxt spn
        SOIStoredProcedureObj spn (SPOCol cn) ->
          "column " <> toTxt spn <> "." <> toTxt cn
        SOILogicalModel lm -> "logical model " <> toTxt lm
        SOILogicalModelObj lm (LMOCol cn) ->
          "logical model column " <> toTxt lm <> "." <> toTxt cn
        SOILogicalModelObj lm (LMOPerm rn pt) ->
          "permission " <> toTxt lm <> "." <> roleNameToTxt rn <> "." <> permTypeToCode pt
        SOILogicalModelObj lm (LMOReferencedLogicalModel inner) ->
          "inner logical model " <> toTxt lm <> "." <> toTxt inner
        SOITableObj tn (TOCol cn) ->
          "column " <> toTxt tn <> "." <> toTxt cn
        SOITableObj tn (TORel cn) ->
          "relationship " <> toTxt tn <> "." <> toTxt cn
        SOITableObj tn (TOForeignKey cn) ->
          "constraint " <> toTxt tn <> "." <> toTxt cn
        SOITableObj tn (TOPerm rn pt) ->
          "permission " <> toTxt tn <> "." <> roleNameToTxt rn <> "." <> permTypeToCode pt
        SOITableObj tn (TOTrigger trn) ->
          "event-trigger " <> toTxt tn <> "." <> triggerNameToTxt trn
        SOITableObj tn (TOComputedField ccn) ->
          "computed field " <> toTxt tn <> "." <> computedFieldNameToText ccn
        SOITableObj tn (TORemoteRel rn) ->
          "remote relationship " <> toTxt tn <> "." <> relNameToTxt rn
  SORemoteSchema remoteSchemaName ->
    "remote schema " <> unNonEmptyText (unRemoteSchemaName remoteSchemaName)
  SORemoteSchemaPermission remoteSchemaName roleName ->
    "remote schema permission "
      <> unNonEmptyText (unRemoteSchemaName remoteSchemaName)
      <> "."
      <>> roleName
  SORemoteSchemaRemoteRelationship remoteSchemaName typeName relationshipName ->
    "remote_relationship "
      <> toTxt relationshipName
      <> " on type "
      <> G.unName typeName
      <> " in remote schema "
      <> toTxt remoteSchemaName
  SORole roleName -> "role " <> roleNameToTxt roleName
  where
    inSource s t = t <> " in source " <>> s

reportSchemaObjs :: [SchemaObjId] -> Text
reportSchemaObjs = commaSeparated . sort . map reportSchemaObj

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
  | DRLogicalModel
  | DRReferencedLogicalModel
  deriving (Show, Eq, Generic)

instance Hashable DependencyReason

reasonToTxt :: DependencyReason -> Text
reasonToTxt = \case
  DRTable -> "table"
  DRColumn -> "column"
  DRRemoteTable -> "remote_table"
  DRLeftColumn -> "left_column"
  DRRightColumn -> "right_column"
  DRUsingColumn -> "using_column"
  DRFkey -> "fkey"
  DRRemoteFkey -> "remote_fkey"
  DRUntyped -> "untyped"
  DROnType -> "on_type"
  DRSessionVariable -> "session_variable"
  DRPayload -> "payload"
  DRParent -> "parent"
  DRRemoteSchema -> "remote_schema"
  DRRemoteRelationship -> "remote_relationship"
  DRParentRole -> "parent_role"
  DRLogicalModel -> "logical_model"
  DRReferencedLogicalModel -> "inner_logical_model"

instance ToJSON DependencyReason where
  toJSON = String . reasonToTxt

data SchemaDependency = SchemaDependency
  { sdObjId :: SchemaObjId,
    sdReason :: DependencyReason
  }
  deriving (Show, Eq, Generic)

instance ToJSON SchemaDependency where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

instance Hashable SchemaDependency

reportDependentObjectsExist :: (MonadError QErr m) => [SchemaObjId] -> m ()
reportDependentObjectsExist dependentObjects =
  throw400 DependencyError
    $ "cannot drop due to the following dependent objects: "
    <> reportSchemaObjs dependentObjects

purgeSourceAndSchemaDependencies ::
  (MonadError QErr m) =>
  SchemaObjId ->
  WriterT MetadataModifier m ()
purgeSourceAndSchemaDependencies = \case
  SOSourceObj sourceName objectID -> do
    AB.dispatchAnyBackend @Backend objectID $ purgeDependentObject sourceName >=> tell
  SORemoteSchemaRemoteRelationship remoteSchemaName typeName relationshipName -> do
    tell $ dropRemoteSchemaRemoteRelationshipInMetadata remoteSchemaName typeName relationshipName
  _ ->
    pure ()

purgeDependentObject ::
  forall b m.
  (MonadError QErr m, Backend b) =>
  SourceName ->
  SourceObjId b ->
  m MetadataModifier
purgeDependentObject source sourceObjId = case sourceObjId of
  SOITableObj tn tableObj ->
    pure
      $ MetadataModifier
      $ tableMetadataSetter @b source tn
      %~ case tableObj of
        TOPerm rn pt -> dropPermissionInMetadata rn pt
        TORel rn -> dropRelationshipInMetadata rn
        TOTrigger trn -> dropEventTriggerInMetadata trn
        TOComputedField ccn -> dropComputedFieldInMetadata ccn
        TORemoteRel rrn -> dropRemoteRelationshipInMetadata rrn
        _ -> id
  SOIFunction qf -> pure $ dropFunctionInMetadata @b source qf
  _ ->
    throw500
      $ "unexpected dependent object: "
      <> reportSchemaObj (SOSourceObj source $ AB.mkAnyBackend sourceObjId)

-- | Type class to collect schema dependencies from backend-specific aggregation predicates.
class (Backend b) => GetAggregationPredicatesDeps b where
  getAggregationPredicateDeps ::
    AggregationPredicates b (PartialSQLExp b) ->
    BoolExpM b [SchemaDependency]
  default getAggregationPredicateDeps ::
    (AggregationPredicates b ~ Const Void) =>
    AggregationPredicates b (PartialSQLExp b) ->
    BoolExpM b [SchemaDependency]
  getAggregationPredicateDeps = absurd . getConst

-- | The monad for doing schema dependency discovery for boolean expressions.
-- maintains the table context of the expressions being translated.
newtype BoolExpM b a = BoolExpM {unBoolExpM :: Reader (BoolExpCtx b) a}
  deriving (Functor, Applicative, Monad, MonadReader (BoolExpCtx b))

-- | The table type context of schema dependency discovery. Boolean expressions
-- may refer to a so-called 'root table' (identified by a '$'-sign in the
-- expression input syntax) or the 'current' table.
data BoolExpCtx b = BoolExpCtx
  { source :: SourceName,
    -- | Reference to the 'current' table type.
    currTable :: TableName b,
    -- | Reference to the 'root' table type.
    rootTable :: TableName b
  }

runBoolExpM :: BoolExpCtx b -> BoolExpM b a -> a
runBoolExpM ctx = flip runReader ctx . unBoolExpM
