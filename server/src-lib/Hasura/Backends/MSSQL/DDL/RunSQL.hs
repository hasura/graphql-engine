{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | MSSQL DDL RunSQL
--
-- Provides primitives for running raw text SQL on MSSQL backends.
module Hasura.Backends.MSSQL.DDL.RunSQL
  ( runSQL,
    MSSQLRunSQL (..),
    isSchemaCacheBuildRequiredRunSQL,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HS
import Data.String (fromString)
import Data.Text qualified as T
import Database.MSSQL.Transaction qualified as Tx
import Database.ODBC.Internal qualified as ODBC
import Database.ODBC.SQLServer qualified as ODBC hiding (query)
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.Meta
import Hasura.Backends.MSSQL.SQL.Error
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Diff
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Metadata
import Hasura.RQL.Types.Metadata.Backend
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.SchemaCache.Build
import Hasura.RQL.Types.SchemaCacheTypes
import Hasura.RQL.Types.Source
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Utils (quoteRegex)
import Hasura.Table.Cache
import Text.Regex.TDFA qualified as TDFA

data MSSQLRunSQL = MSSQLRunSQL
  { _mrsSql :: Text,
    _mrsSource :: SourceName,
    _mrsCascade :: Bool,
    _mrsCheckMetadataConsistency :: Maybe Bool
  }
  deriving (Show, Eq)

instance J.FromJSON MSSQLRunSQL where
  parseJSON = J.withObject "MSSQLRunSQL" $ \o -> do
    _mrsSql <- o J..: "sql"
    _mrsSource <- o J..:? "source" J..!= defaultSource
    _mrsCascade <- o J..:? "cascade" J..!= False
    _mrsCheckMetadataConsistency <- o J..:? "check_metadata_consistency"
    pure MSSQLRunSQL {..}

instance J.ToJSON MSSQLRunSQL where
  toJSON MSSQLRunSQL {..} =
    J.object
      [ "sql" J..= _mrsSql,
        "source" J..= _mrsSource,
        "cascade" J..= _mrsCascade,
        "check_metadata_consistency" J..= _mrsCheckMetadataConsistency
      ]

runSQL ::
  forall m.
  (MonadIO m, MonadBaseControl IO m, CacheRWM m, MonadError QErr m, MetadataM m) =>
  MSSQLRunSQL ->
  m EncJSON
runSQL mssqlRunSQL@MSSQLRunSQL {..} = do
  SourceInfo {..} <- askSourceInfo @'MSSQL _mrsSource
  results <-
    -- If the SQL modifies the schema of the database then check for any metadata changes
    if isSchemaCacheBuildRequiredRunSQL mssqlRunSQL
      then do
        (results, metadataUpdater) <- runTx _siConfiguration $ withMetadataCheck _siTables
        -- Build schema cache with updated metadata
        withNewInconsistentObjsCheck
          $ buildSchemaCacheWithInvalidations mempty {ciSources = HS.singleton _mrsSource} metadataUpdater
        pure results
      else runTx _siConfiguration sqlQueryTx
  pure $ encJFromJValue $ toResult results
  where
    runTx :: SourceConfig 'MSSQL -> Tx.TxET QErr m a -> m a
    runTx sourceConfig =
      liftEitherM . runMSSQLSourceWriteTx sourceConfig

    sqlQueryTx :: Tx.TxET QErr m [[(ODBC.Column, ODBC.Value)]]
    sqlQueryTx =
      Tx.buildGenericQueryTxE runSqlMSSQLTxErrorHandler _mrsSql textToODBCQuery ODBC.query
      where
        textToODBCQuery :: Text -> ODBC.Query
        textToODBCQuery = fromString . T.unpack

        runSqlMSSQLTxErrorHandler :: Tx.MSSQLTxError -> QErr
        runSqlMSSQLTxErrorHandler =
          -- The SQL query is user provided. Capture all error classes as expected exceptions.
          mkMSSQLTxErrorHandler (const True)

    withMetadataCheck ::
      TableCache 'MSSQL ->
      Tx.TxET QErr m ([[(ODBC.Column, ODBC.Value)]], MetadataModifier)
    withMetadataCheck tableCache = do
      preActionTablesMeta <- toTableMeta <$> loadDBMetadata
      results <- sqlQueryTx
      postActionTablesMeta <- toTableMeta <$> loadDBMetadata
      let trackedTablesMeta = filter (flip HashMap.member tableCache . tmTable) preActionTablesMeta
          tablesDiff = getTablesDiff trackedTablesMeta postActionTablesMeta

      -- Get indirect dependencies
      indirectDeps <- getIndirectDependenciesFromTableDiff _mrsSource tablesDiff
      -- Report indirect dependencies, if any, when cascade is not set
      unless (null indirectDeps || _mrsCascade) $ reportDependentObjectsExist indirectDeps

      metadataUpdater <- execWriterT $ do
        -- Purge all the indirect dependents from state
        for_ indirectDeps \case
          SOSourceObj sourceName objectID -> do
            AB.dispatchAnyBackend @BackendMetadata objectID $ purgeDependentObject sourceName >=> tell
          _ ->
            pure ()
        processTablesDiff _mrsSource tableCache tablesDiff

      pure (results, metadataUpdater)
      where
        toTableMeta :: DBTablesMetadata 'MSSQL -> [TableMeta 'MSSQL]
        toTableMeta dbTablesMeta =
          HashMap.toList dbTablesMeta <&> \(table, dbTableMeta) ->
            TableMeta table dbTableMeta [] -- No computed fields

isSchemaCacheBuildRequiredRunSQL :: MSSQLRunSQL -> Bool
isSchemaCacheBuildRequiredRunSQL MSSQLRunSQL {..} =
  fromMaybe (sqlContainsDDLKeyword _mrsSql) _mrsCheckMetadataConsistency
  where
    sqlContainsDDLKeyword :: Text -> Bool
    sqlContainsDDLKeyword =
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
              "\\balter\\b|\\bdrop\\b|\\bsp_rename\\b"
          )

toResult :: [[(ODBC.Column, ODBC.Value)]] -> RunSQLRes
toResult result = case result of
  [] -> RunSQLRes "CommandOk" J.Null
  (firstRow : _) -> RunSQLRes "TuplesOk" $ J.toJSON $ toHeader firstRow : toRows result
  where
    toRows = map $ map $ odbcValueToJValue . snd
    toHeader = map $ J.String . ODBC.columnName . fst
