-- |
-- Description: Add/Drop computed fields in metadata
module Hasura.RQL.DDL.ComputedField
  ( AddComputedField (..),
    ComputedFieldDefinition (..),
    runAddComputedField,
    DropComputedField,
    runDropComputedField,
    dropComputedFieldInMetadata,
  )
where

import Data.Aeson
import Data.HashMap.Strict.InsOrd qualified as OMap
import Data.Text.Extended
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.Permission
import Hasura.RQL.Types
import Hasura.SQL.AnyBackend qualified as AB

data AddComputedField b = AddComputedField
  { _afcSource :: !SourceName,
    _afcTable :: !(TableName b),
    _afcName :: !ComputedFieldName,
    _afcDefinition :: !(ComputedFieldDefinition b),
    _afcComment :: !Comment
  }
  deriving stock (Generic)

instance (Backend b) => ToJSON (AddComputedField b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (AddComputedField b) where
  parseJSON = withObject "AddComputedField" $ \o ->
    AddComputedField
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "name"
      <*> o .: "definition"
      <*> o .:? "comment" .!= Automatic

runAddComputedField ::
  forall b m.
  (BackendMetadata b, MonadError QErr m, CacheRWM m, MetadataM m) =>
  AddComputedField b ->
  m EncJSON
runAddComputedField q = do
  void $ withPathK "table" $ askTabInfo @b source table
  let metadataObj =
        MOSourceObjId source $
          AB.mkAnyBackend $
            SMOTableObj @b table $
              MTOComputedField computedFieldName
      metadata = ComputedFieldMetadata computedFieldName (_afcDefinition q) (_afcComment q)
  buildSchemaCacheFor metadataObj $
    MetadataModifier $
      tableMetadataSetter source table . tmComputedFields
        %~ OMap.insert computedFieldName metadata
  pure successMsg
  where
    source = _afcSource q
    table = _afcTable q
    computedFieldName = _afcName q

data DropComputedField b = DropComputedField
  { _dccSource :: !SourceName,
    _dccTable :: !(TableName b),
    _dccName :: !ComputedFieldName,
    _dccCascade :: !Bool
  }

instance (Backend b) => FromJSON (DropComputedField b) where
  parseJSON = withObject "DropComputedField" $ \o ->
    DropComputedField
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "name"
      <*> o .:? "cascade" .!= False

runDropComputedField ::
  forall b m.
  (QErrM m, CacheRWM m, MetadataM m, BackendMetadata b) =>
  DropComputedField b ->
  m EncJSON
runDropComputedField (DropComputedField source table computedField cascade) = do
  -- Validation
  fields <- withPathK "table" $ _tciFieldInfoMap <$> askTableCoreInfo @b source table
  void $ withPathK "name" $ askComputedFieldInfo fields computedField

  -- Dependencies check
  sc <- askSchemaCache
  let deps =
        getDependentObjs sc $
          SOSourceObj source $
            AB.mkAnyBackend $
              SOITableObj @b table $
                TOComputedField computedField
  when (not cascade && not (null deps)) $ reportDependentObjectsExist deps

  withNewInconsistentObjsCheck do
    metadataModifiers <- mapM purgeComputedFieldDependency deps
    buildSchemaCache $
      MetadataModifier $
        tableMetadataSetter @b source table
          %~ dropComputedFieldInMetadata computedField . foldl' (.) id metadataModifiers
  pure successMsg
  where
    purgeComputedFieldDependency = \case
      -- TODO: do a better check of ensuring that the dependency is as expected.
      -- i.e, the only allowed dependent objects on a computed fields are permissions
      -- on the same table
      SOSourceObj _ exists
        | Just (SOITableObj _ (TOPerm roleName permType)) <-
            AB.unpackAnyBackend @b exists ->
          pure $ dropPermissionInMetadata roleName permType
      d ->
        throw500 $
          "unexpected dependency for computed field "
            <> computedField <<> "; "
            <> reportSchemaObj d
