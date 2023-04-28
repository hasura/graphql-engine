module Hasura.UpgradeTests.Dataset
  ( Dataset,
    datasetName,
    datasetExpectedTypeCount,
    mkDataset,
    datasetMigrationSql,
    datasetReplaceMetadataCommand,
  )
where

import Codec.Compression.GZip qualified as GZip
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as ByteString
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Encoding qualified as Text
import Hasura.Prelude

-- | A dataset which can be loaded into the database and tracked.
data Dataset = Dataset
  { datasetName :: String,
    datasetPath :: FilePath,
    datasetExpectedTypeCount :: Int
  }

-- | Constructs a new dataset.
mkDataset :: FilePath -> String -> Int -> Dataset
mkDataset repositoryRoot datasetName datasetExpectedTypeCount = Dataset {..}
  where
    datasetPath =
      repositoryRoot
        <> "/server/benchmarks/benchmark_sets/"
        <> datasetName

-- | Reads the migration SQL for the given dataset.
datasetMigrationSql :: Dataset -> IO Text
datasetMigrationSql dataset =
  Text.toStrict . Text.decodeLatin1 . GZip.decompress <$> ByteString.readFile dumpPath
  where
    dumpPath = datasetPath dataset <> "/dump.sql.gz"

-- | Reads the replace metadata JSON for the given dataset.
datasetReplaceMetadataCommand :: Dataset -> IO J.Value
datasetReplaceMetadataCommand dataset =
  (J.decode <$> ByteString.readFile metadataJsonPath)
    >>= (`onNothing` (fail "Invalid metadata JSON"))
  where
    metadataJsonPath = datasetPath dataset <> "/replace_metadata.json"
