-- | Functions for loading and modifying the catalog. See the module documentation for
-- "Hasura.RQL.DDL.Schema" for more details.
module Hasura.RQL.DDL.Schema.Catalog
  ( buildCatalogMetadata
  , fetchTableMetadataFromDb
  , fetchFunctionMetadataFromDb
  -- , fetchCatalogData
  -- , saveTableToCatalog
  -- , updateTableIsEnumInCatalog
  -- , updateTableConfig
  -- , deleteTableFromCatalog
  -- , getTableConfig
  , purgeDependentObject
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                as HM
import qualified Data.HashSet                       as HS
import qualified Database.PG.Query                  as Q

import           Control.Lens                       (ix)
import           Data.Aeson
import           Data.List                          (unzip6)

import           Hasura.Db
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Permission.Internal
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types
import           Hasura.RQL.Types.Catalog
import           Hasura.SQL.Types


buildCatalogMetadata
  :: forall m. (MonadTx m)
  => Metadata -> m CatalogMetadata
buildCatalogMetadata Metadata{..} = do
  catalogTables <- fetchTableMetadataFromDb
  let functions = map fst allFunctions <> concatMap
        (map (_cfdFunction . _cfmDefinition) . HM.elems . _tmComputedFields) (HM.elems _metaTables)
  catalogFunctions <- fetchFunctionMetadataFromDb functions
  pgScalars <- fetchPgScalars

  let _cmFunctions = flip map allFunctions $ \(function, config) ->
        CatalogFunction function notSystemDefined config $ fromMaybe [] $
        HM.lookup function catalogFunctions
      (_cmTables, relations, computedFields, remoteRelations, permissions, eventTriggers) =
        unzip6 $ flip map (HM.elems _metaTables) $ \table ->
        transformTable table catalogFunctions $ HM.lookup (_tmTable table) catalogTables
      _cmRelations            = concat relations
      _cmComputedFields       = concat computedFields
      _cmRemoteRelationships  = concat remoteRelations
      _cmPermissions          = concat permissions
      _cmEventTriggers        = concat eventTriggers
      _cmActions              = HM.elems _metaActions
      _cmCronTriggers         = map transformCronTrigger $ HM.elems _metaCronTriggers
      _cmRemoteSchemas        = HM.elems _metaRemoteSchemas
      _cmCustomTypes          = CatalogCustomTypes _metaCustomTypes pgScalars
      _cmAllowlistCollections = map _ccDefinition $
        filter (flip HS.member _metaAllowlist . CollectionReq . _ccName)
        (HM.elems _metaQueryCollections)
  pure CatalogMetadata{..}
  where
    notSystemDefined = SystemDefined False
    allFunctions = case _metaFunctions of
      FMVersion1 v1Functions -> map (, emptyFunctionConfig) $ toList v1Functions
      FMVersion2 v2Functions -> map ( _tfv2Function &&& _tfv2Configuration) $ HM.elems v2Functions

    transformCronTrigger :: CronTriggerMetadata -> CatalogCronTrigger
    transformCronTrigger CronTriggerMetadata{..} =
      CatalogCronTrigger ctName ctWebhook ctSchedule ctPayload
      (Just ctRetryConf) (Just ctHeaders) ctComment

    transformTable
      :: TableMetadata
      -> HM.HashMap QualifiedFunction [RawFunctionInfo]
      -> Maybe CatalogTableInfo
      -> ( CatalogTable
         , [CatalogRelation]
         , [CatalogComputedField]
         , [RemoteRelationship]
         , [CatalogPermission]
         , [CatalogEventTrigger]
         )
    transformTable TableMetadata{..} catalogFunction tableInfo =
      let catalogTable = CatalogTable _tmTable notSystemDefined _tmIsEnum
                         _tmConfiguration tableInfo
          relationships = map (transformRelation ObjRel) (HM.elems _tmObjectRelationships)
                          <> map (transformRelation ArrRel) (HM.elems _tmArrayRelationships)
          computedFields = map transformComputedField $ HM.elems _tmComputedFields
          remoteRelations = map transformRemoteRelation $ HM.elems _tmRemoteRelationships
          permissions = map (transformPermission PTSelect) (HM.elems _tmSelectPermissions)
                        <> map (transformPermission PTInsert) (HM.elems _tmInsertPermissions)
                        <> map (transformPermission PTUpdate) (HM.elems _tmUpdatePermissions)
                        <> map (transformPermission PTDelete) (HM.elems _tmDeletePermissions)
          eventTriggers = map transformEventTrigger $ HM.elems _tmEventTriggers
      in (catalogTable, relationships, computedFields, remoteRelations, permissions, eventTriggers)
      where
        transformRelation :: ToJSON a => RelType -> RelDef a -> CatalogRelation
        transformRelation relType RelDef{..} =
          CatalogRelation _tmTable _rdName relType (toJSON _rdUsing) _rdComment

        transformComputedField :: ComputedFieldMetadata -> CatalogComputedField
        transformComputedField ComputedFieldMetadata{..} =
          let computedField = AddComputedField _tmTable _cfmName _cfmDefinition _cfmComment
          in CatalogComputedField computedField $
             fromMaybe [] $ HM.lookup (_cfdFunction _cfmDefinition) catalogFunction

        transformRemoteRelation :: RemoteRelationshipMeta -> RemoteRelationship
        transformRemoteRelation RemoteRelationshipMeta{..} =
          let RemoteRelationshipDef{..} = _rrmDefinition
          in RemoteRelationship _rrmName _tmTable _rrdHasuraFields _rrdRemoteSchema _rrdRemoteField

        transformPermission :: ToJSON a => PermType -> PermDef a -> CatalogPermission
        transformPermission permType PermDef{..} =
          CatalogPermission _tmTable _pdRole permType (toJSON _pdPermission) _pdComment

        transformEventTrigger :: EventTriggerConf -> CatalogEventTrigger
        transformEventTrigger etc =
          CatalogEventTrigger _tmTable (etcName etc) $ toJSON etc


    fetchPgScalars :: m (HS.HashSet PGScalarType)
    fetchPgScalars =
      liftTx $ Q.getAltJ . runIdentity . Q.getRow
      <$> Q.withQE defaultTxErrorHandler
      [Q.sql|
        SELECT coalesce(json_agg(typname), '[]')
        FROM pg_catalog.pg_type where typtype = 'b'
      |] () True

-- | Fetch all user tables with metadata
fetchTableMetadataFromDb
  :: (MonadTx m) => m (HM.HashMap QualifiedTable CatalogTableInfo)
fetchTableMetadataFromDb = do
  results <- liftTx $ Q.withQE defaultTxErrorHandler
             $(Q.sqlFromFile "src-rsr/pg_table_metadata.sql")
             () True
  pure $ HM.fromList $ flip map results $
    \(schema, table, Q.AltJ info) -> (QualifiedObject schema table, info)

-- | Fetch Postgres metadata for the given functions
fetchFunctionMetadataFromDb
  :: (MonadTx m)
  => [QualifiedFunction] -> m (HM.HashMap QualifiedFunction [RawFunctionInfo])
fetchFunctionMetadataFromDb functionList = do
  results <- liftTx $ Q.withQE defaultTxErrorHandler
             $(Q.sqlFromFile "src-rsr/pg_function_metadata.sql")
             (Identity $ Q.AltJ $ toJSON functionList) True
  pure $ HM.fromList $ flip map results $
    \(schema, table, Q.AltJ infos) -> (QualifiedObject schema table, infos)

-- fetchCatalogData :: (MonadTx m) => m CatalogMetadata
-- fetchCatalogData =
--   liftTx $ Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
--   $(Q.sqlFromFile "src-rsr/catalog_metadata.sql") () True

purgeDependentObject
  :: (MonadError QErr m) => SchemaObjId -> m MetadataModifier
purgeDependentObject = \case
  SOTableObj tn tableObj -> pure $ MetadataModifier $
    metaTables.ix tn %~ case tableObj of
      TOPerm rn pt        -> dropPermissionInMetadata rn pt
      TORel rn rt         -> dropRelationshipInMetadata rn rt
      TOTrigger trn       -> dropEventTriggerInMetadata trn
      TOComputedField ccn -> dropComputedFieldInMetadata ccn
      TORemoteRel rrn     -> dropRemoteRelationshipInMetadata rrn
      _                   -> id
  SOFunction qf         -> pure $ dropFunctionInMetadata qf
  schemaObjId           ->
      throw500 $ "unexpected dependent object: " <> reportSchemaObj schemaObjId

-- saveTableToCatalog
--   :: (MonadTx m, HasSystemDefined m) => QualifiedTable -> Bool -> TableConfig -> m ()
-- saveTableToCatalog (QualifiedObject sn tn) isEnum config = do
--   systemDefined <- askSystemDefined
--   liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
--     INSERT INTO "hdb_catalog"."hdb_table"
--       (table_schema, table_name, is_system_defined, is_enum, configuration)
--     VALUES ($1, $2, $3, $4, $5)
--   |] (sn, tn, systemDefined, isEnum, configVal) False
--   where
--     configVal = Q.AltJ $ toJSON config

-- updateTableIsEnumInCatalog :: (MonadTx m) => QualifiedTable -> Bool -> m ()
-- updateTableIsEnumInCatalog (QualifiedObject sn tn) isEnum = liftTx $
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--       UPDATE "hdb_catalog"."hdb_table" SET is_enum = $3
--       WHERE table_schema = $1 AND table_name = $2
--     |] (sn, tn, isEnum) False

-- updateTableConfig :: (MonadTx m) => QualifiedTable -> TableConfig -> m ()
-- updateTableConfig (QualifiedObject sn tn) config = liftTx $
--   Q.unitQE defaultTxErrorHandler [Q.sql|
--            UPDATE "hdb_catalog"."hdb_table"
--               SET configuration = $1
--             WHERE table_schema = $2 AND table_name = $3
--                 |] (configVal, sn, tn) False
--   where
--     configVal = Q.AltJ $ toJSON config

-- deleteTableFromCatalog :: (MonadTx m) => QualifiedTable -> m ()
-- deleteTableFromCatalog (QualifiedObject sn tn) = liftTx $ Q.unitQE defaultTxErrorHandler [Q.sql|
--     DELETE FROM "hdb_catalog"."hdb_table"
--     WHERE table_schema = $1 AND table_name = $2
--   |] (sn, tn) False

-- getTableConfig :: MonadTx m => QualifiedTable -> m TableConfig
-- getTableConfig (QualifiedObject sn tn) = liftTx $
--   Q.getAltJ . runIdentity . Q.getRow <$> Q.withQE defaultTxErrorHandler
--     [Q.sql|
--        SELECT configuration::json FROM hdb_catalog.hdb_table
--         WHERE table_schema = $1 AND table_name = $2
--     |] (sn, tn) True
