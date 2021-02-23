module Hasura.Backends.MSSQL.DDL.Source
  ( resolveSourceConfig
  , resolveDatabaseMetadata
  , postDropSourceHook
  )
where

import           Hasura.Prelude

import           Control.Exception

import qualified Database.ODBC.SQLServer          as ODBC

import           Hasura.Backends.MSSQL.Connection
import           Hasura.Backends.MSSQL.Meta
import           Hasura.Backends.MSSQL.Types
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.RQL.Types.Source
import           Hasura.SQL.Backend

resolveSourceConfig
  :: (MonadIO m)
  => SourceName
  -> MSSQLConnConfiguration
  -> m (Either QErr MSSQLSourceConfig)
resolveSourceConfig _name config = runExceptT do
  eitherResult <- liftIO $ try $ ODBC.connect connStringText
  case eitherResult of
    Left (e :: SomeException) ->
      throw400 Unexpected $ "unexpected exception while connecting to database: " <> tshow e
    Right conn ->
      pure $ MSSQLSourceConfig connString conn
  where
    MSSQLConnConfiguration connInfo = config
    connString = _mciConnectionString connInfo
    connStringText = unMSSQLConnectionString connString

resolveDatabaseMetadata
  :: (MonadIO m)
  => MSSQLSourceConfig
  -> m (Either QErr (ResolvedSource 'MSSQL))
resolveDatabaseMetadata config = runExceptT do
  eitherResult <- liftIO $ try $ loadDBMetadata conn
  case eitherResult of
    Left (e :: SomeException) ->
      throw400 Unexpected $ "unexpected exception while connecting to database: " <> tshow e
    Right dbTablesMetadata -> do
      pure $ ResolvedSource config dbTablesMetadata mempty mempty
  where
    MSSQLSourceConfig _connString conn = config

postDropSourceHook
  :: (MonadIO m)
  => MSSQLSourceConfig -> m ()
postDropSourceHook (MSSQLSourceConfig _ conn) =
  -- Close the connection
  ODBC.close conn
