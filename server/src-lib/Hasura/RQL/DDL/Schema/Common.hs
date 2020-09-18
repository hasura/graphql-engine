module Hasura.RQL.DDL.Schema.Common where

import           Hasura.Prelude
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types

purgeDependentObject
  :: (MonadError QErr m) => SchemaObjId -> m MetadataModifier
purgeDependentObject = \case
  SOSourceObj source (SOITableObj tn tableObj) -> pure $ MetadataModifier $
    tableMetadataSetter source tn %~ case tableObj of
      TOPerm rn pt        -> dropPermissionInMetadata rn pt
      TORel rn rt         -> dropRelationshipInMetadata rn rt
      TOTrigger trn       -> dropEventTriggerInMetadata trn
      TOComputedField ccn -> dropComputedFieldInMetadata ccn
      TORemoteRel rrn     -> dropRemoteRelationshipInMetadata rrn
      _                   -> id
  SOSourceObj source (SOIFunction qf) -> pure $ dropFunctionInMetadata source qf
  schemaObjId           ->
      throw500 $ "unexpected dependent object: " <> reportSchemaObj schemaObjId
