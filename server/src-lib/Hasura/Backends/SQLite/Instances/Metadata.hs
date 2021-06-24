{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Hasura.Backends.SQLite.Instances.Metadata () where

import           Hasura.Prelude

import qualified Data.ByteString.UTF8                   as BS
import qualified Database.SQLite.Simple                 as L

import           Control.Exception
import           Control.Monad.Trans.Control            (MonadBaseControl)
import           Data.FileEmbed                         (embedFile, makeRelativeToProject)
import           Data.HashMap.Strict.Extended           (groupOn)
import           Data.String

import           Hasura.Backends.SQLite.Instances.Types ()
import           Hasura.Backends.SQLite.Types
import           Hasura.Base.Error
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Metadata.Backend
import           Hasura.RQL.Types.Source
import           Hasura.RQL.Types.Table
import           Hasura.SQL.Backend


-- instance

instance BackendMetadata 'SQLite where
  resolveSourceConfig        = slResolveSourceConfig
  resolveDatabaseMetadata    = slResolveDatabaseMetadata

  buildComputedFieldInfo     = undefined
  fetchAndValidateEnumValues = undefined
  createTableEventTrigger    = undefined
  buildEventTriggerInfo      = undefined
  parseBoolExpOperations     = undefined
  buildFunctionInfo          = undefined
  updateColumnInEventTrigger = undefined
  parseCollectableType       = undefined
  postDropSourceHook         = undefined


-- functions

slResolveSourceConfig
  :: (MonadIO m, MonadBaseControl IO m)
  => SourceName
  -> SLFilePath
  -> m (Either QErr SLSourceConfig)
slResolveSourceConfig _ p@(SLFilePath path) =
  liftIO $ createConfig  `catch` \(e :: SomeException) -> pure $ throw400 NotFound $ tshow e
  where
    createConfig = do
      c <- L.open path
      pure $ Right $ SLSourceConfig p c

slResolveDatabaseMetadata
  :: (MonadIO m)
  => SLSourceConfig
  -> m (Either QErr (ResolvedSource 'SQLite))
slResolveDatabaseMetadata config@(SLSourceConfig _ connection) = runExceptT do
  let queryBytes  = $(makeRelativeToProject "src-rsr/sqlite_table_metadata.sql" >>= embedFile)
      sqliteQuery = fromString . BS.toString $ queryBytes
  rows <- liftIO $ groupOn _slTableName <$> L.query_ connection sqliteQuery
  let tables = flip evalState 0 $ for rows \columns -> do
        i <- get
        put $ succ i
        pure $  DBTableMetadata @'SQLite
          (OID i) -- SQLite doesn't have table OIDs
          [ RawColumnInfo name position kind (not nonNullable) Nothing
          | SLMetaRow _ name position kind nonNullable <- columns
          ]
          Nothing -- no PK information
          mempty  -- no unique constraints
          mempty  -- no foreign keys
          Nothing -- no views
          Nothing -- no description
          ()
  pure $ ResolvedSource config tables mempty mempty


-- helpers

data SLMetaRow = SLMetaRow
  { _slTableName     :: Text
  , _slColumnName    :: Text
  , _slColumnIndex   :: Int
  , _slColumnType    :: Text
  , _slColumnNotNull :: Bool
  }

instance L.FromRow SLMetaRow where
  fromRow = SLMetaRow <$> L.field <*> L.field <*> L.field <*> L.field <*> L.field
