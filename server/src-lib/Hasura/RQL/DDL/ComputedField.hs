{- |
Description: Add/Drop computed fields in metadata
-}
module Hasura.RQL.DDL.ComputedField
  ( AddComputedField(..)
  , ComputedFieldDefinition(..)
  , runAddComputedField
  , DropComputedField
  , runDropComputedField
  , dropComputedFieldInMetadata
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict.InsOrd as OMap

import           Data.Aeson
import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend      as AB

import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.Incremental         (Cacheable)
import           Hasura.RQL.DDL.Deps
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.Types


data AddComputedField b
  = AddComputedField
  { _afcSource     :: !SourceName
  , _afcTable      :: !(TableName b)
  , _afcName       :: !ComputedFieldName
  , _afcDefinition :: !(ComputedFieldDefinition b)
  , _afcComment    :: !(Maybe Text)
  } deriving (Generic)
deriving instance (Backend b) => Show (AddComputedField b)
deriving instance (Backend b) => Eq (AddComputedField b)
instance (Backend b) => NFData (AddComputedField b)
instance (Backend b) => Cacheable (AddComputedField b)
instance (Backend b) => ToJSON (AddComputedField b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (AddComputedField b) where
  parseJSON = withObject "Object" $ \o ->
    AddComputedField
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "name"
      <*> o .: "definition"
      <*> o .:? "comment"

runAddComputedField
  :: forall b m
   . (BackendMetadata b, MonadError QErr m, CacheRWM m, MetadataM m)
  => AddComputedField b
  -> m EncJSON
runAddComputedField q = do
  withPathK "table" $ askTabInfo @b source table
  let metadataObj = MOSourceObjId source
                      $ AB.mkAnyBackend
                      $ SMOTableObj @b table
                      $ MTOComputedField computedFieldName
      metadata = ComputedFieldMetadata computedFieldName (_afcDefinition q) (_afcComment q)
  buildSchemaCacheFor metadataObj
    $ MetadataModifier
    $ tableMetadataSetter source table.tmComputedFields
      %~ OMap.insert computedFieldName metadata
  pure successMsg
  where
    source = _afcSource q
    table = _afcTable q
    computedFieldName = _afcName q

data DropComputedField b
  = DropComputedField
  { _dccSource  :: !SourceName
  , _dccTable   :: !(TableName b)
  , _dccName    :: !ComputedFieldName
  , _dccCascade :: !Bool
  } deriving (Generic)
deriving instance (Backend b) => Show (DropComputedField b)
deriving instance (Backend b) => Eq (DropComputedField b)
instance (Backend b) => ToJSON (DropComputedField b) where
  toJSON = genericToJSON hasuraJSON

instance (Backend b) => FromJSON (DropComputedField b) where
  parseJSON = withObject "Object" $ \o ->
    DropComputedField
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "name"
      <*> o .:? "cascade" .!= False

runDropComputedField
  :: forall b m
   . (QErrM m, CacheRWM m, MetadataM m, BackendMetadata b)
  => DropComputedField b -> m EncJSON
runDropComputedField (DropComputedField source table computedField cascade) = do
  -- Validation
  fields <- withPathK "table" $ _tciFieldInfoMap <$> askTableCoreInfo @b source table
  void $ withPathK "name" $ askComputedFieldInfo fields computedField

  -- Dependencies check
  sc <- askSchemaCache
  let deps = getDependentObjs sc
               $ SOSourceObj source
               $ AB.mkAnyBackend
               $ SOITableObj @b table
               $ TOComputedField computedField
  when (not cascade && not (null deps)) $ reportDeps deps

  withNewInconsistentObjsCheck do
    metadataModifiers <- mapM purgeComputedFieldDependency deps
    buildSchemaCache $ MetadataModifier $
      tableMetadataSetter @b source table
      %~ dropComputedFieldInMetadata computedField . foldl' (.) id metadataModifiers
  pure successMsg
  where
    purgeComputedFieldDependency = \case
      -- TODO: do a better check of ensuring that the dependency is as expected.
      -- i.e, the only allowed dependent objects on a computed fields are permissions
      -- on the same table
      SOSourceObj _ exists
        | Just (SOITableObj _ (TOPerm roleName permType))
            <- AB.unpackAnyBackend @b exists ->
              pure $ dropPermissionInMetadata roleName permType
      d -> throw500 $ "unexpected dependency for computed field "
           <> computedField <<> "; " <> reportSchemaObj d

dropComputedFieldInMetadata
  :: ComputedFieldName -> TableMetadata b -> TableMetadata b
dropComputedFieldInMetadata name =
  tmComputedFields %~ OMap.delete name
