module Hasura.RQL.DDL.Schema.Common where

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Prelude
import           Hasura.RQL.DDL.ComputedField
import           Hasura.RQL.DDL.EventTrigger
import           Hasura.RQL.DDL.Permission
import           Hasura.RQL.DDL.Relationship
import           Hasura.RQL.DDL.RemoteRelationship
import           Hasura.RQL.DDL.Schema.Function
import           Hasura.RQL.Types

import qualified Data.HashMap.Strict                as HM
import qualified Database.PG.Query                  as Q

purgeDependentObject
  :: (MonadError QErr m) => SchemaObjId -> m MetadataModifier
purgeDependentObject = \case
  SOSourceObj source (SOITableObj tn tableObj) -> pure $ MetadataModifier $
    tableMetadataSetter source tn %~ case tableObj of
      TOPerm rn pt        -> dropPermissionInMetadata rn pt
      TORel rn            -> dropRelationshipInMetadata rn
      TOTrigger trn       -> dropEventTriggerInMetadata trn
      TOComputedField ccn -> dropComputedFieldInMetadata ccn
      TORemoteRel rrn     -> dropRemoteRelationshipInMetadata rrn
      _                   -> id
  SOSourceObj source (SOIFunction qf) -> pure $ dropFunctionInMetadata source qf
  schemaObjId           ->
      throw500 $ "unexpected dependent object: " <> reportSchemaObj schemaObjId

-- | Fetch Postgres metadata of all user tables
fetchTableMetadata :: (MonadTx m) => m (DBTablesMetadata 'Postgres)
fetchTableMetadata = do
  results <- liftTx $ Q.withQE defaultTxErrorHandler
             $(Q.sqlFromFile "src-rsr/pg_table_metadata.sql") () True
  pure $ HM.fromList $ flip map results $
    \(schema, table, Q.AltJ info) -> (QualifiedObject schema table, info)

-- | Fetch Postgres metadata for all user functions
fetchFunctionMetadata :: (MonadTx m) => m PostgresFunctionsMetadata
fetchFunctionMetadata = do
  results <- liftTx $ Q.withQE defaultTxErrorHandler
             $(Q.sqlFromFile "src-rsr/pg_function_metadata.sql") () True
  pure $ HM.fromList $ flip map results $
    \(schema, table, Q.AltJ infos) -> (QualifiedObject schema table, infos)

-- | Fetch all scalar types from Postgres
fetchPgScalars :: MonadTx m => m (HashSet PGScalarType)
fetchPgScalars =
  liftTx $ Q.getAltJ . runIdentity . Q.getRow
  <$> Q.withQE defaultTxErrorHandler
  [Q.sql|
    SELECT coalesce(json_agg(typname), '[]')
    FROM pg_catalog.pg_type where typtype = 'b'
   |] () True
