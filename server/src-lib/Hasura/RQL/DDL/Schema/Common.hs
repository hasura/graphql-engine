module Hasura.RQL.DDL.Schema.Common where

import           Hasura.Prelude

import qualified Hasura.SQL.AnyBackend             as AB

import           Hasura.Base.Error
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types


purgeDependentObject
  :: forall b m
   . (MonadError QErr m, BackendMetadata b)
  => SourceName -> SourceObjId b -> m MetadataModifier
purgeDependentObject source sourceObjId = case sourceObjId of
  SOITableObj tn tableObj -> pure $ MetadataModifier $
    tableMetadataSetter @b source tn %~ case tableObj of
      TOPerm rn pt        -> dropPermissionInMetadata rn pt
      TORel rn            -> dropRelationshipInMetadata rn
      TOTrigger trn       -> dropEventTriggerInMetadata trn
      TOComputedField ccn -> dropComputedFieldInMetadata ccn
      TORemoteRel rrn     -> dropRemoteRelationshipInMetadata rrn
      _                   -> id
  SOIFunction qf -> pure $ dropFunctionInMetadata @b source qf
  _           ->
    throw500
      $ "unexpected dependent object: "
      <> reportSchemaObj (SOSourceObj source $ AB.mkAnyBackend sourceObjId)
