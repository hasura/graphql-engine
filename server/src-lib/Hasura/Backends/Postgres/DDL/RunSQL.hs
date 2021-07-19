module Hasura.Backends.Postgres.DDL.RunSQL
  ( runRunSQL
  , RunSQL(..)
  , isSchemaCacheBuildRequiredRunSQL
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as M
import qualified Data.HashSet                        as HS
import qualified Database.PG.Query                   as Q
import qualified Text.Regex.TDFA                     as TDFA

import           Control.Monad.Trans.Control         (MonadBaseControl)
import           Data.Aeson
import           Data.Text.Extended

import qualified Hasura.SQL.AnyBackend               as AB

import           Hasura.Backends.Postgres.DDL.Source (ToMetadataFetchQuery, fetchFunctionMetadata,
                                                      fetchTableMetadata)
import           Hasura.Backends.Postgres.DDL.Table
import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Base.Error
import           Hasura.EncJSON
import           Hasura.RQL.DDL.Deps                 (reportDepsExt)
import           Hasura.RQL.DDL.Schema
import           Hasura.RQL.DDL.Schema.Common
import           Hasura.RQL.DDL.Schema.Diff
import           Hasura.RQL.Types                    hiding (ConstraintName, fmFunction,
                                                      tmComputedFields, tmTable)
import           Hasura.Server.Utils                 (quoteRegex)


data RunSQL
  = RunSQL
  { rSql                      :: Text
  , rSource                   :: !SourceName
  , rCascade                  :: !Bool
  , rCheckMetadataConsistency :: !(Maybe Bool)
  , rTxAccessMode             :: !Q.TxAccess
  } deriving (Show, Eq)

instance FromJSON RunSQL where
  parseJSON = withObject "RunSQL" $ \o -> do
    rSql <- o .: "sql"
    rSource <- o .:? "source" .!= defaultSource
    rCascade <- o .:? "cascade" .!= False
    rCheckMetadataConsistency <- o .:? "check_metadata_consistency"
    isReadOnly <- o .:? "read_only" .!= False
    let rTxAccessMode = if isReadOnly then Q.ReadOnly else Q.ReadWrite
    pure RunSQL{..}

instance ToJSON RunSQL where
  toJSON RunSQL {..} =
    object
      [ "sql" .= rSql
      , "source" .= rSource
      , "cascade" .= rCascade
      , "check_metadata_consistency" .= rCheckMetadataConsistency
      , "read_only" .=
        case rTxAccessMode of
          Q.ReadOnly  -> True
          Q.ReadWrite -> False
      ]

-- | see Note [Checking metadata consistency in run_sql]
isSchemaCacheBuildRequiredRunSQL :: RunSQL -> Bool
isSchemaCacheBuildRequiredRunSQL RunSQL {..} =
  case rTxAccessMode of
    Q.ReadOnly  -> False
    Q.ReadWrite -> fromMaybe (containsDDLKeyword rSql) rCheckMetadataConsistency
    where
      containsDDLKeyword = TDFA.match $$(quoteRegex
        TDFA.defaultCompOpt
          { TDFA.caseSensitive = False
          , TDFA.multiline = True
          , TDFA.lastStarGreedy = True }
          TDFA.defaultExecOpt
          { TDFA.captureGroups = False }
          "\\balter\\b|\\bdrop\\b|\\breplace\\b|\\bcreate function\\b|\\bcomment on\\b")


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

fetchMeta
  :: (ToMetadataFetchQuery pgKind, BackendMetadata ('Postgres pgKind), MonadTx m)
  => TableCache ('Postgres pgKind)
  -> FunctionCache ('Postgres pgKind)
  -> m ([TableMeta ('Postgres pgKind)], [FunctionMeta ('Postgres pgKind)])
fetchMeta tables functions = do
  tableMetaInfos <- fetchTableMetadata
  functionMetaInfos <- fetchFunctionMetadata

  let getFunctionMetas function =
        let mkFunctionMeta rawInfo =
              FunctionMeta (rfiOid rawInfo) function (rfiFunctionType rawInfo)
        in maybe [] (map mkFunctionMeta) $ M.lookup function functionMetaInfos

      mkComputedFieldMeta computedField =
        let function = _cffName $ _cfiFunction computedField
        in map (ComputedFieldMeta (_cfiName computedField)) $ getFunctionMetas function

      tableMetas = flip map (M.toList tableMetaInfos) $ \(table, tableMetaInfo) ->
                   TableMeta table tableMetaInfo $ fromMaybe [] $
                     M.lookup table tables <&> \tableInfo ->
                     let tableCoreInfo  = _tiCoreInfo tableInfo
                         computedFields = getComputedFieldInfos $ _tciFieldInfoMap tableCoreInfo
                     in  concatMap mkComputedFieldMeta computedFields

      functionMetas = concatMap getFunctionMetas $ M.keys functions

  pure (tableMetas, functionMetas)

runRunSQL
  :: forall (pgKind :: PostgresKind) m
   . ( BackendMetadata ('Postgres pgKind)
     , ToMetadataFetchQuery pgKind
     , CacheRWM m
     , HasServerConfigCtx m
     , MetadataM m
     , MonadBaseControl IO m
     , MonadError QErr m
     , MonadIO m
     )
  => RunSQL
  -> m EncJSON
runRunSQL q@RunSQL {..}
  -- see Note [Checking metadata consistency in run_sql]
  | isSchemaCacheBuildRequiredRunSQL q
  = withMetadataCheck @pgKind rSource rCascade rTxAccessMode $ execRawSQL rSql
  | otherwise
  = askSourceConfig @('Postgres pgKind) rSource >>= \sourceConfig ->
      liftEitherM $ runExceptT $
      runLazyTx (_pscExecCtx sourceConfig) rTxAccessMode $ execRawSQL rSql
  where
    execRawSQL :: (MonadTx n) => Text -> n EncJSON
    execRawSQL =
      fmap (encJFromJValue @RunSQLRes) . liftTx . Q.multiQE rawSqlErrHandler . Q.fromText
      where
        rawSqlErrHandler txe =
          (err400 PostgresError "query execution failed") { qeInternal = Just $ toJSON txe }

-- | @'withMetadataCheck' cascade action@ runs @action@ and checks if the schema changed as a
-- result. If it did, it checks to ensure the changes do not violate any integrity constraints, and
-- if not, incorporates them into the schema cache.
withMetadataCheck
  :: forall (pgKind :: PostgresKind) a m
   . ( BackendMetadata ('Postgres pgKind)
     , ToMetadataFetchQuery pgKind
     , CacheRWM m
     , HasServerConfigCtx m
     , MetadataM m
     , MonadBaseControl IO m
     , MonadError QErr m
     , MonadIO m
     )
  => SourceName -> Bool -> Q.TxAccess -> LazyTxT QErr m a -> m a
withMetadataCheck source cascade txAccess action = do
  SourceInfo _ preActionTables preActionFunctions sourceConfig <- askSourceInfo @('Postgres pgKind) source

  (actionResult, metadataUpdater) <-
    liftEitherM $ runExceptT $ runLazyTx (_pscExecCtx sourceConfig) txAccess $ do
      -- Drop event triggers so no interference is caused to the sql query
      forM_ (M.elems preActionTables) $ \tableInfo -> do
        let eventTriggers = _tiEventTriggerInfoMap tableInfo
        forM_ (M.keys eventTriggers) (liftTx . delTriggerQ)

      -- Get the metadata before the sql query, everything, need to filter this
      (preActionTableMeta, preActionFunctionMeta) <- fetchMeta preActionTables preActionFunctions

      -- Run the action
      actionResult <- action
      -- Get the metadata after the sql query
      (postActionTableMeta, postActionFunctionMeta) <- fetchMeta preActionTables preActionFunctions

      let preActionTableMeta' = filter (flip M.member preActionTables . tmTable) preActionTableMeta
          schemaDiff = getSchemaDiff preActionTableMeta' postActionTableMeta
          FunctionDiff droppedFuncs alteredFuncs = getFuncDiff preActionFunctionMeta postActionFunctionMeta
          overloadedFuncs = getOverloadedFuncs (M.keys preActionFunctions) postActionFunctionMeta

      -- Do not allow overloading functions
      unless (null overloadedFuncs) $
        throw400 NotSupported $ "the following tracked function(s) cannot be overloaded: "
        <> commaSeparated overloadedFuncs

      indirectSourceDeps <- getSchemaChangeDeps source schemaDiff

      let indirectDeps =
            map
              (SOSourceObj source . AB.mkAnyBackend)
              indirectSourceDeps
      -- Report back with an error if cascade is not set
      when (indirectDeps /= [] && not cascade) $ reportDepsExt indirectDeps []

      metadataUpdater <- execWriterT $ do
        -- Purge all the indirect dependents from state
        mapM_ (purgeDependentObject source >=> tell) indirectSourceDeps

        -- Purge all dropped functions
        let purgedFuncs = flip mapMaybe indirectSourceDeps $ \case
              SOIFunction qf -> Just qf
              _              -> Nothing

        forM_ (droppedFuncs \\ purgedFuncs) $ tell . dropFunctionInMetadata @('Postgres pgKind) source

        -- Process altered functions
        forM_ alteredFuncs $ \(qf, newTy) -> do
          when (newTy == FTVOLATILE) $
            throw400 NotSupported $
            "type of function " <> qf <<> " is altered to \"VOLATILE\" which is not supported now"

        -- update the metadata with the changes
        processSchemaDiff source preActionTables schemaDiff

      pure (actionResult, metadataUpdater)

  -- Build schema cache with updated metadata
  withNewInconsistentObjsCheck $
    buildSchemaCacheWithInvalidations mempty{ciSources = HS.singleton source} metadataUpdater

  postActionSchemaCache <- askSchemaCache

  -- Recreate event triggers in hdb_catalog
  let postActionTables = fromMaybe mempty $ unsafeTableCache @('Postgres pgKind) source $ scSources postActionSchemaCache
  serverConfigCtx <- askServerConfigCtx
  liftEitherM $ runPgSourceWriteTx sourceConfig $
    forM_ (M.elems postActionTables) $ \(TableInfo coreInfo _ eventTriggers) -> do
      let table = _tciName coreInfo
          columns = getCols $ _tciFieldInfoMap coreInfo
      forM_ (M.toList eventTriggers) $ \(triggerName, eti) -> do
        let opsDefinition = etiOpsDef eti
        flip runReaderT serverConfigCtx $ mkAllTriggersQ triggerName table columns opsDefinition

  pure actionResult
