module Hasura.Server.Migrate.Version
  ( MetadataCatalogVersion (..),
    SourceCatalogVersion (..),
    SourceCatalogMigrationState (..),
  )
where

import Data.Aeson qualified as J
import Data.List (isPrefixOf)
import Data.Text.Extended
import Hasura.Logging (Hasura, LogLevel (..), ToEngineLog (..))
import Hasura.Prelude
import Hasura.RQL.Types.BackendType (BackendType)
import Hasura.RQL.Types.Common (SourceName)
import Hasura.Server.Logging (StartupLog (..))
import Language.Haskell.TH.Lift (Lift)

-- | Represents the catalog version. This is stored in the database and then
-- compared with the latest version on startup.
data MetadataCatalogVersion
  = -- | A typical catalog version.
    MetadataCatalogVersion Int
  | -- | Maintained for compatibility with catalog version 0.8.
    MetadataCatalogVersion08
  deriving stock (Eq, Lift)

instance Ord MetadataCatalogVersion where
  compare = compare `on` toFloat
    where
      toFloat :: MetadataCatalogVersion -> Float
      toFloat (MetadataCatalogVersion v) = fromIntegral v
      toFloat MetadataCatalogVersion08 = 0.8

instance Enum MetadataCatalogVersion where
  toEnum = MetadataCatalogVersion
  fromEnum (MetadataCatalogVersion v) = v
  fromEnum MetadataCatalogVersion08 = error "Cannot enumerate unstable catalog versions."

instance Show MetadataCatalogVersion where
  show (MetadataCatalogVersion v) = show v
  show MetadataCatalogVersion08 = "0.8"

instance Read MetadataCatalogVersion where
  readsPrec prec s
    | "0.8" `isPrefixOf` s =
        [(MetadataCatalogVersion08, drop 3 s)]
    | otherwise =
        map (first MetadataCatalogVersion) $ filter ((>= 0) . fst) $ readsPrec @Int prec s

-- | This is the source catalog version, used when deciding whether to (re-)create event triggers.
newtype SourceCatalogVersion (backend :: BackendType) = SourceCatalogVersion {unSourceCatalogVersion :: Int}
  deriving newtype (Eq, Enum, Show, Read)
  deriving stock (Lift)

data SourceCatalogMigrationState
  = -- | Source has not been initialized yet.
    SCMSUninitializedSource
  | -- | Source catalog is already at the latest catalog version.
    SCMSNothingToDo Int
  | -- | Initialization of the source catalog along with the catalog version.
    SCMSInitialized Int
  | -- | Source catalog migration <old catalog version> to <new catalog version>.
    SCMSMigratedTo Int Int
  | -- | Source catalog migration on hold with reason (Maintenance mode, read only mode etc).
    SCMSMigrationOnHold Text
  | SCMSNotSupported

instance ToEngineLog (SourceName, SourceCatalogMigrationState) Hasura where
  toEngineLog (sourceName, migrationStatus) =
    let migrationStatusMessage =
          case migrationStatus of
            SCMSUninitializedSource -> "source " <> sourceName <<> " has not been initialized yet."
            SCMSNothingToDo catalogVersion ->
              "source "
                <> sourceName
                <<> " is already at the latest catalog version ("
                <> tshow catalogVersion
                <> ")."
            SCMSInitialized catalogVersion ->
              "source "
                <> sourceName
                <<> " has the source catalog version successfully initialized (at version "
                <> tshow catalogVersion
                <> ")."
            SCMSMigratedTo oldCatalogVersion newCatalogVersion ->
              "source "
                <> sourceName
                <<> " has been migrated successfully from catalog version "
                <> tshow oldCatalogVersion
                <> " to "
                <> tshow newCatalogVersion
                <> "."
            SCMSMigrationOnHold reason ->
              "Source catalog migration for source: " <> sourceName <<> " is on hold due to " <> reason <> "."
            SCMSNotSupported ->
              "Source catalog migration is not supported for source " <>> sourceName
     in toEngineLog
          $ StartupLog
            { slLogLevel = LevelInfo,
              slKind = "source_catalog_migrate",
              slInfo =
                J.toJSON
                  $ J.object
                    [ "source" J..= sourceName,
                      "message" J..= migrationStatusMessage
                    ]
            }
