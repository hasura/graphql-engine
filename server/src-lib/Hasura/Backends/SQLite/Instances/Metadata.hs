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
    createConfig = undefined -- TODO

slResolveDatabaseMetadata
  :: (MonadIO m)
  => SLSourceConfig
  -> m (Either QErr (ResolvedSource 'SQLite))
slResolveDatabaseMetadata config = undefined -- TODO
