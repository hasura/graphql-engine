{-# LANGUAGE ViewPatterns #-}

module Hasura.Backends.MSSQL.DDL.RunSQL
  ( runSQL,
    MSSQLRunSQL (..),
    isSchemaCacheBuildRequiredRunSQL,
  )
where

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson qualified as J
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as HS
import Data.String (fromString)
import Data.Text qualified as T
import Database.MSSQL.Transaction qualified as Tx
import Database.ODBC.Internal qualified as ODBC
import Hasura.Backends.MSSQL.Connection
import Hasura.Backends.MSSQL.Meta
import Hasura.Base.Error
import Hasura.EncJSON
import Hasura.Prelude
import Hasura.RQL.DDL.Schema
import Hasura.RQL.DDL.Schema.Diff
import Hasura.RQL.Types hiding (TableName, runTx, tmTable)
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Server.Utils (quoteRegex)
import Text.Regex.TDFA qualified as TDFA

data MSSQLRunSQL = MSSQLRunSQL
  { _mrsSql :: Text,
    _mrsSource :: !SourceName,
    _mrsCascade :: !Bool,
    _mrsCheckMetadataConsistency :: !(Maybe Bool)
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
  (MonadIO m, MonadBaseControl IO m, CacheRWM m, MonadError QErr m, MetadataM m) =>
  MSSQLRunSQL ->
  m EncJSON
runSQL mssqlRunSQL@MSSQLRunSQL {..} = do
  SourceInfo _ tableCache _ sourceConfig _ _ <- askSourceInfo @'MSSQL _mrsSource
  let pool = _mscConnectionPool sourceConfig
  results <- withMSSQLPool pool $ \conn ->
    -- If the SQL modifies the schema of the database then check for any metadata changes
    if isSchemaCacheBuildRequiredRunSQL mssqlRunSQL
      then do
        (results, metadataUpdater) <- withMetadataCheck tableCache conn
        -- Build schema cache with updated metadata
        withNewInconsistentObjsCheck $
          buildSchemaCacheWithInvalidations mempty {ciSources = HS.singleton _mrsSource} metadataUpdater
        pure results
      else liftEitherM $ runExceptT $ runTx conn sqlQueryTx
  pure $ encJFromJValue $ toResult results
  where
    runTx :: (MonadIO m) => ODBC.Connection -> Tx.TxET QErr m a -> ExceptT QErr m a
    runTx conn tx = Tx.runTxE fromMSSQLTxError tx conn

    sqlQueryTx :: (MonadIO m) => Tx.TxET QErr m [[(ODBC.Column, ODBC.Value)]]
    sqlQueryTx =
      Tx.buildGenericTxE fromODBCException (\conn -> ODBC.query conn $ fromString $ T.unpack _mrsSql)
      where
        fromODBCException :: ODBC.ODBCException -> QErr
        fromODBCException e =
          (err400 MSSQLError "sql query exception")
            { qeInternal = Just (ExtraInternal $ odbcExceptionToJSONValue e)
            }

    withMetadataCheck ::
      (MonadIO m, CacheRWM m, MonadError QErr m) =>
      TableCache 'MSSQL ->
      ODBC.Connection ->
      m ([[(ODBC.Column, ODBC.Value)]], MetadataModifier)
    withMetadataCheck tableCache conn = liftEitherM $
      runExceptT $
        runTx conn $ do
          preActionTablesMeta <- toTableMeta <$> loadDBMetadata
          results <- sqlQueryTx
          postActionTablesMeta <- toTableMeta <$> loadDBMetadata
          let trackedTablesMeta = filter (flip M.member tableCache . tmTable) preActionTablesMeta
              tablesDiff = getTablesDiff trackedTablesMeta postActionTablesMeta

          -- Get indirect dependencies
          indirectDeps <- getIndirectDependencies _mrsSource tablesDiff
          -- Report indirect dependencies, if any, when cascade is not set
          when (indirectDeps /= [] && not _mrsCascade) $ reportDependentObjectsExist indirectDeps

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
          M.toList dbTablesMeta <&> \(table, dbTableMeta) ->
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
