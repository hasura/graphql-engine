{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Postgres DDL RunSQL
--
-- Escape hatch for running raw SQL against a postgres database.
--
-- 'runRunSQL' executes the provided raw SQL.
--
-- 'isSchemaCacheBuildRequiredRunSQL' checks for known schema-mutating keywords
-- in the raw SQL text.
--
-- See 'Hasura.Server.API.V2Query' and 'Hasura.Server.API.Query'.
module Hasura.Backends.Postgres.DDL.RunSQL
  ( runRunSQL,
    RunSQL (..),
    isSchemaCacheBuildRequiredRunSQL,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as HS
import Data.Text.Extended
import Database.PG.Query qualified as Q
import Hasura.Backends.Postgres.Connection.MonadTx
import Hasura.Backends.Postgres.DDL.EventTrigger
import Hasura.Backends.Postgres.DDL.Source
  ( ToMetadataFetchQuery,
    fetchFunctionMetadata,
    fetchTableMetadata,
  )
import Hasura.Backends.Postgres.Execute.Types
import Hasura.Backends.Postgres.SQL.Types hiding (FunctionName, TableName)
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Diff
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.EventTrigger
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Metadata hiding
  ( fmFunction,
    tmComputedFields,
    tmTable,
  )
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.Table
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.SQL.Backend
import Hasura.Server.Types
import Hasura.Server.Utils (quoteRegex)
import Hasura.Session
import Hasura.Tracing qualified as Tracing
import Text.Regex.TDFA qualified as TDFA

data RunSQL = RunSQL
  { rSql :: Text,
    rSource :: !SourceName,
    rCascade :: !Bool,
    rCheckMetadataConsistency :: !(Maybe Bool),
    rTxAccessMode :: !Q.TxAccess
  }
  deriving (Show, Eq)

instance FromJSON RunSQL where
  parseJSON = withObject "RunSQL" $ \o -> do
    rSql <- o .: "sql"
    rSource <- o .:? "source" .!= defaultSource
    rCascade <- o .:? "cascade" .!= False
    rCheckMetadataConsistency <- o .:? "check_metadata_consistency"
    isReadOnly <- o .:? "read_only" .!= False
    let rTxAccessMode = if isReadOnly then Q.ReadOnly else Q.ReadWrite
    pure RunSQL {..}

instance ToJSON RunSQL where
  toJSON RunSQL {..} =
    object
      [ "sql" .= rSql,
        "source" .= rSource,
        "cascade" .= rCascade,
        "check_metadata_consistency" .= rCheckMetadataConsistency,
        "read_only"
          .= case rTxAccessMode of
            Q.ReadOnly -> True
            Q.ReadWrite -> False
      ]

-- | Check for known schema-mutating keywords in the raw SQL text.
--
-- See Note [Checking metadata consistency in run_sql].
isSchemaCacheBuildRequiredRunSQL :: RunSQL -> Bool
isSchemaCacheBuildRequiredRunSQL RunSQL {..} =
  case rTxAccessMode of
    Q.ReadOnly -> False
    Q.ReadWrite -> fromMaybe (containsDDLKeyword rSql) rCheckMetadataConsistency
  where
    containsDDLKeyword =
      TDFA.match
        $$( quoteRegex
              TDFA.defaultCompOpt
                { TDFA.caseSensitive = False,
                  TDFA.multiline = True,
                  TDFA.lastStarGreedy = True
                }
              TDFA.defaultExecOpt
                { TDFA.captureGroups = False
                }
              "\\balter\\b|\\bdrop\\b|\\breplace\\b|\\bcreate function\\b|\\bcomment on\\b"
          )

{- Note [Checking metadata consistency in run_sql]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SQL queries executed by run_sql may change the Postgres schema in arbitrary
ways. We attempt to automatically update the metadata to reflect those changes
as much as possible---for example, if a table is renamed, we want to update the
metadata to track the table under its new name instead of its old one. This
schema diffing (plus some integrity checking) is handled by withMetadataCheck.

But this process has overhead---it involves reloading the metadata, diffing it,
and rebuilding the schema cache---so we don’t want to do it if it isn’t
necessary. The user can explicitly disable the check via the
check_metadata_consistency option, and we also skip it if the current
transaction is in READ ONLY mode, since the schema can’t be modified in that
case, anyway.

However, even if neither read_only or check_metadata_consistency is passed, lots
of queries may not modify the schema at all. As a (fairly stupid) heuristic, we
check if the query contains any keywords for DDL operations, and if not, we skip
the metadata check as well. -}

-- | Fetch metadata of tracked tables/functions and build @'TableMeta'/@'FunctionMeta'
-- to calculate diff later in @'withMetadataCheck'.
fetchTablesFunctionsMetadata ::
  (ToMetadataFetchQuery pgKind, BackendMetadata ('Postgres pgKind), MonadTx m) =>
  TableCache ('Postgres pgKind) ->
  [TableName ('Postgres pgKind)] ->
  [FunctionName ('Postgres pgKind)] ->
  m ([TableMeta ('Postgres pgKind)], [FunctionMeta ('Postgres pgKind)])
fetchTablesFunctionsMetadata tableCache tables functions = do
  tableMetaInfos <- fetchTableMetadata tables
  functionMetaInfos <- fetchFunctionMetadata functions
  pure (buildTableMeta tableMetaInfos functionMetaInfos, buildFunctionMeta functionMetaInfos)
  where
    buildTableMeta tableMetaInfos functionMetaInfos =
      flip map (M.toList tableMetaInfos) $ \(table, tableMetaInfo) ->
        TableMeta table tableMetaInfo $
          foldMap @Maybe (concatMap (mkComputedFieldMeta functionMetaInfos) . getComputedFields) (M.lookup table tableCache)

    buildFunctionMeta functionMetaInfos =
      concatMap (getFunctionMetas functionMetaInfos) functions

    mkComputedFieldMeta functionMetaInfos computedField =
      let function = _cffName $ _cfiFunction computedField
       in map (ComputedFieldMeta (_cfiName computedField)) $ getFunctionMetas functionMetaInfos function

    getFunctionMetas functionMetaInfos function =
      let mkFunctionMeta rawInfo =
            FunctionMeta (rfiOid rawInfo) function (rfiFunctionType rawInfo)
       in foldMap @Maybe (map mkFunctionMeta) $ M.lookup function functionMetaInfos

-- | Used as an escape hatch to run raw SQL against a database.
runRunSQL ::
  forall (pgKind :: PostgresKind) m.
  ( BackendMetadata ('Postgres pgKind),
    ToMetadataFetchQuery pgKind,
    CacheRWM m,
    HasServerConfigCtx m,
    MetadataM m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadIO m,
    Tracing.MonadTrace m,
    UserInfoM m
  ) =>
  RunSQL ->
  m EncJSON
runRunSQL q@RunSQL {..} = do
  sourceConfig <- askSourceConfig @('Postgres pgKind) rSource
  traceCtx <- Tracing.currentContext
  userInfo <- askUserInfo
  let pgExecCtx = _pscExecCtx sourceConfig
  if (isSchemaCacheBuildRequiredRunSQL q)
    then do
      -- see Note [Checking metadata consistency in run_sql]
      withMetadataCheck @pgKind rSource rCascade rTxAccessMode $
        withTraceContext traceCtx $
          withUserInfo userInfo $
            execRawSQL rSql
    else do
      runTxWithCtx pgExecCtx rTxAccessMode $ execRawSQL rSql
  where
    execRawSQL :: (MonadTx n) => Text -> n EncJSON
    execRawSQL =
      fmap (encJFromJValue @RunSQLRes) . liftTx . Q.multiQE rawSqlErrHandler . Q.fromText
      where
        rawSqlErrHandler txe =
          (err400 PostgresError "query execution failed") {qeInternal = Just $ ExtraInternal $ toJSON txe}

-- | @'withMetadataCheck' source cascade txAccess runSQLQuery@ executes @runSQLQuery@ and checks if the schema changed as a
-- result. If it did, it checks to ensure the changes do not violate any integrity constraints, and
-- if not, incorporates them into the schema cache.
-- TODO(antoine): shouldn't this be generalized?
withMetadataCheck ::
  forall (pgKind :: PostgresKind) a m.
  ( BackendMetadata ('Postgres pgKind),
    ToMetadataFetchQuery pgKind,
    CacheRWM m,
    HasServerConfigCtx m,
    MetadataM m,
    MonadBaseControl IO m,
    MonadError QErr m,
    MonadIO m
  ) =>
  SourceName ->
  Bool ->
  Q.TxAccess ->
  Q.TxET QErr m a ->
  m a
withMetadataCheck source cascade txAccess runSQLQuery = do
  SourceInfo _ tableCache functionCache sourceConfig _ _ <- askSourceInfo @('Postgres pgKind) source

  -- Run SQL query and metadata checker in a transaction
  (queryResult, metadataUpdater) <- runTxWithMetadataCheck source sourceConfig txAccess tableCache functionCache cascade runSQLQuery

  -- Build schema cache with updated metadata
  withNewInconsistentObjsCheck $
    buildSchemaCacheWithInvalidations mempty {ciSources = HS.singleton source} metadataUpdater

  postRunSQLSchemaCache <- askSchemaCache

  -- Recreate event triggers in hdb_catalog. Event triggers are dropped before executing @'runSQLQuery'.
  recreateEventTriggers sourceConfig postRunSQLSchemaCache

  pure queryResult
  where
    recreateEventTriggers :: PGSourceConfig -> SchemaCache -> m ()
    recreateEventTriggers sourceConfig schemaCache = do
      let tables = fromMaybe mempty $ unsafeTableCache @('Postgres pgKind) source $ scSources schemaCache
      serverConfigCtx <- askServerConfigCtx
      liftEitherM $
        runPgSourceWriteTx sourceConfig $
          forM_ (M.elems tables) $ \(TableInfo coreInfo _ eventTriggers _) -> do
            let table = _tciName coreInfo
                columns = getCols $ _tciFieldInfoMap coreInfo
            forM_ (M.toList eventTriggers) $ \(triggerName, eti) -> do
              let opsDefinition = etiOpsDef eti
              flip runReaderT serverConfigCtx $ mkAllTriggersQ triggerName table columns opsDefinition

-- | @'runTxWithMetadataCheck source sourceConfig txAccess tableCache functionCache cascadeDependencies tx' checks for
-- changes in GraphQL Engine metadata when a @'tx' is executed on the database alters Postgres
-- schema of tables and functions. If any indirect dependencies (Eg. remote table dependence of a relationship) are
-- found and @'cascadeDependencies' is False, then an exception is raised.
runTxWithMetadataCheck ::
  forall m a (pgKind :: PostgresKind).
  ( BackendMetadata ('Postgres pgKind),
    ToMetadataFetchQuery pgKind,
    CacheRWM m,
    MonadIO m,
    MonadBaseControl IO m,
    MonadError QErr m
  ) =>
  SourceName ->
  SourceConfig ('Postgres pgKind) ->
  Q.TxAccess ->
  TableCache ('Postgres pgKind) ->
  FunctionCache ('Postgres pgKind) ->
  Bool ->
  Q.TxET QErr m a ->
  m (a, MetadataModifier)
runTxWithMetadataCheck source sourceConfig txAccess tableCache functionCache cascadeDependencies tx =
  liftEitherM $
    runExceptT $
      runTx (_pscExecCtx sourceConfig) txAccess $ do
        -- Running in a transaction helps to rollback the @'tx' execution in case of any exceptions

        -- Before running the @'tx', fetch metadata of existing tables and functions from Postgres.
        let tableNames = M.keys tableCache
            computedFieldFunctions = concatMap getComputedFieldFunctions (M.elems tableCache)
            functionNames = M.keys functionCache <> computedFieldFunctions
        (preTxTablesMeta, preTxFunctionsMeta) <- fetchTablesFunctionsMetadata tableCache tableNames functionNames

        -- Since the @'tx' may alter table/function names we use the OIDs of underlying tables
        -- (sourced from 'pg_class' for tables and 'pg_proc' for functions), which remain unchanged in the
        -- case if a table/function is renamed.
        let tableOids = map (_ptmiOid . tmInfo) preTxTablesMeta
            functionOids = map fmOid preTxFunctionsMeta

        -- Run the transaction
        txResult <- tx

        (postTxTablesMeta, postTxFunctionMeta) <-
          uncurry (fetchTablesFunctionsMetadata tableCache)
            -- Fetch names of tables and functions using OIDs which also contains renamed items
            =<< fetchTablesFunctionsFromOids tableOids functionOids

        -- Calculate the tables diff (dropped & altered tables)
        let tablesDiff = getTablesDiff preTxTablesMeta postTxTablesMeta
            -- Calculate the functions diff. For calculating diff for functions, only consider
            -- query/mutation functions and exclude functions underpinning computed fields.
            -- Computed field functions are being processed under each table diff.
            -- See @'getTablesDiff' and @'processTablesDiff'
            excludeComputedFieldFunctions = filter ((`M.member` functionCache) . fmFunction)
            functionsDiff =
              getFunctionsDiff
                (excludeComputedFieldFunctions preTxFunctionsMeta)
                (excludeComputedFieldFunctions postTxFunctionMeta)

        dontAllowFunctionOverloading $
          getOverloadedFunctions
            (M.keys functionCache)
            (excludeComputedFieldFunctions postTxFunctionMeta)

        -- Update metadata with schema change caused by @'tx'
        metadataUpdater <- execWriterT do
          -- Collect indirect dependencies of altered tables
          tableIndirectDeps <- getIndirectDependenciesFromTableDiff source tablesDiff

          -- If table indirect dependencies exist and cascading is not enabled then report an exception
          when (not (null tableIndirectDeps) && not cascadeDependencies) $ reportDependentObjectsExist tableIndirectDeps

          -- Purge all the table dependents
          traverse_ purgeSourceAndSchemaDependencies tableIndirectDeps

          -- Collect function names from purged table dependencies
          let purgedFunctions = collectFunctionsInDeps tableIndirectDeps
              FunctionsDiff droppedFunctions alteredFunctions = functionsDiff

          -- Drop functions in metadata. Exclude functions that were already dropped as part of table indirect dependencies
          purgeFunctionsFromMetadata $ droppedFunctions \\ purgedFunctions

          -- If any function type is altered to VOLATILE then raise an exception
          dontAllowFunctionAlteredVolatile alteredFunctions

          -- Propagate table changes to metadata
          processTablesDiff source tableCache tablesDiff

        pure (txResult, metadataUpdater)
  where
    dontAllowFunctionOverloading ::
      MonadError QErr n =>
      [FunctionName ('Postgres pgKind)] ->
      n ()
    dontAllowFunctionOverloading overloadedFunctions =
      unless (null overloadedFunctions) $
        throw400 NotSupported $
          "the following tracked function(s) cannot be overloaded: "
            <> commaSeparated overloadedFunctions

    dontAllowFunctionAlteredVolatile ::
      MonadError QErr n =>
      [(FunctionName ('Postgres pgKind), FunctionVolatility)] ->
      n ()
    dontAllowFunctionAlteredVolatile alteredFunctions =
      forM_ alteredFunctions $ \(qf, newTy) -> do
        when (newTy == FTVOLATILE) $
          throw400 NotSupported $
            "type of function " <> qf <<> " is altered to \"VOLATILE\" which is not supported now"

    purgeFunctionsFromMetadata ::
      Monad n =>
      [FunctionName ('Postgres pgKind)] ->
      WriterT MetadataModifier n ()
    purgeFunctionsFromMetadata functions =
      for_ functions $ tell . dropFunctionInMetadata @('Postgres pgKind) source

    collectFunctionsInDeps :: [SchemaObjId] -> [FunctionName ('Postgres pgKind)]
    collectFunctionsInDeps deps =
      flip mapMaybe deps \case
        SOSourceObj _ objectID
          | Just (SOIFunction qf) <- AB.unpackAnyBackend @('Postgres pgKind) objectID ->
            Just qf
        _ -> Nothing

-- | Fetch list of tables and functions with provided oids
fetchTablesFunctionsFromOids ::
  (MonadIO m) =>
  [OID] ->
  [OID] ->
  Q.TxET QErr m ([TableName ('Postgres pgKind)], [FunctionName ('Postgres pgKind)])
fetchTablesFunctionsFromOids tableOids functionOids =
  ((Q.getAltJ *** Q.getAltJ) . Q.getRow)
    <$> Q.withQE
      defaultTxErrorHandler
      [Q.sql|
    SELECT
      COALESCE(
        ( SELECT
            json_agg(
              row_to_json(
                (
                  SELECT e
                    FROM ( SELECT "table".relname AS "name",
                                  "schema".nspname AS "schema"
                    ) AS e
                )
              )
            ) AS "item"
            FROM jsonb_to_recordset($1::jsonb) AS oid_table("oid" int)
                 JOIN pg_catalog.pg_class "table" ON ("table".oid = "oid_table".oid)
                 JOIN pg_catalog.pg_namespace "schema" ON ("schema".oid = "table".relnamespace)
        ),
        '[]'
      ) AS "tables",

      COALESCE(
        ( SELECT
            json_agg(
              row_to_json(
                (
                  SELECT e
                    FROM ( SELECT "function".proname AS "name",
                                  "schema".nspname AS "schema"
                    ) AS e
                )
              )
            ) AS "item"
            FROM jsonb_to_recordset($2::jsonb) AS oid_table("oid" int)
                 JOIN pg_catalog.pg_proc "function" ON ("function".oid = "oid_table".oid)
                 JOIN pg_catalog.pg_namespace "schema" ON ("schema".oid = "function".pronamespace)
        ),
        '[]'
      ) AS "functions"
  |]
      (Q.AltJ $ map mkOidObject tableOids, Q.AltJ $ map mkOidObject functionOids)
      True
  where
    mkOidObject oid = object ["oid" .= oid]

------ helpers ------------

getComputedFields :: TableInfo ('Postgres pgKind) -> [ComputedFieldInfo ('Postgres pgKind)]
getComputedFields = getComputedFieldInfos . _tciFieldInfoMap . _tiCoreInfo

getComputedFieldFunctions :: TableInfo ('Postgres pgKind) -> [FunctionName ('Postgres pgKind)]
getComputedFieldFunctions = map (_cffName . _cfiFunction) . getComputedFields
