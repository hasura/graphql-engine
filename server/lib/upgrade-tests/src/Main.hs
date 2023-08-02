{-# LANGUAGE TemplateHaskell #-}

-- | These tests ensure that upgrading HGE preserves the GraphQL schema.
--
-- They do this by running two different versions of HGE against the sme
-- metadata, and ensuring that the GraphQL schema doesn't change.
--
-- We might find that in the future, we make an additive change that makes these
-- tests fail. Improving the tests to allow for this is left as an exercise to
-- whoever triggers it. (Sorry.)
--
-- Currently, we do this with:
--
--   * an empty database (zero tracked relations)
--   * the Chinook dataset
--   * the "huge schema" dataset
--
-- The base version of HGE tested against can be overridden with an option. The
-- version must be available on Docker Hub.
module Main (main) where

import Data.Aeson ((.=))
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as J.KeyMap
import Data.ByteString.Lazy qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as ByteString.Char8
import Data.FileEmbed (embedFile, makeRelativeToProject)
import Data.Text.Lazy.Encoding qualified as Text
import Data.Vector qualified as Vector
import Harness.Http qualified as Http
import Harness.Yaml (shouldBeYaml)
import Hasura.Prelude
import Hasura.UpgradeTests.Database
import Hasura.UpgradeTests.Dataset
import Hasura.UpgradeTests.Options
import Hasura.UpgradeTests.Server
import System.Environment (withArgs)
import Test.Hspec
import TestContainers.Hspec qualified as TC

main :: IO ()
main = do
  options <- parseOptions
  withArgs (optionsHspecArgs options)
    . hspec
    -- we just run a single database container for all tests
    . aroundAll
      ( TC.withContainers do
          network <- TC.createNetwork TC.networkRequest
          database <- dbContainer network
          pure (network, database)
      )
    $ spec options

-- | The various tests.
--
-- They do the following:
--
--   1. Start a PostgreSQL database to act as the metadata and source database.
--   2. Add some relations to the database (using the benchmark sets).
--   3. Spin up the latest released version of HGE as a Docker container,
--      pointing to this database.
--   4. Track the aforementioned relations.
--   5. Dump the full GraphQL schema using introspection.gql.
--   6. Check that there are enough types in the schema, to make sure metadata
--      has loaded correctly.
--   7. Shut down HGE and start the current version, using the test harness.
--   8. Dump the schema again.
--   9. Ensure the two GraphQL schemas match.
--
-- This takes a little while, but doesn't require running hordes of queries or
-- actually loading data, so should be quite reliable.
spec :: Options -> SpecWith (TC.Network, Database)
spec options = describe "upgrading HGE" do
  let repositoryRoot = optionsRepositoryRoot options
      datasets =
        [ mkDataset repositoryRoot "chinook" 400,
          mkDataset repositoryRoot "huge_schema" 8000
        ]

  it "works with an empty schema" \(network, database) -> do
    databaseSchema <- newSchema database

    baseSchema <- withBaseHge network baseVersion databaseSchema \server -> do
      Http.postValue (serverGraphqlUrl server) mempty introspectionQuery

    baseSchemaTypeLength <- typeLength baseSchema
    baseSchemaTypeLength `shouldSatisfy` (> 10)

    currentSchema <- withCurrentHge databaseSchema \server -> do
      Http.postValue (serverGraphqlUrl server) mempty introspectionQuery

    currentSchema `shouldBeYaml` baseSchema

  forM_ datasets \dataset -> do
    it ("works with the " <> show (datasetName dataset) <> " dataset") \(network, database) -> do
      migrationSql <- datasetMigrationSql dataset
      replaceMetadataCommand <- datasetReplaceMetadataCommand dataset

      databaseSchema <- newSchema database
      runSql (databaseSchemaUrlForHost databaseSchema) migrationSql

      baseSchema <- withBaseHge network baseVersion databaseSchema \server -> do
        void $ Http.postValue (serverMetadataUrl server) mempty replaceMetadataCommand
        Http.postValue (serverGraphqlUrl server) mempty introspectionQuery

      baseSchemaTypeLength <- typeLength baseSchema
      baseSchemaTypeLength `shouldSatisfy` (> datasetExpectedTypeCount dataset)

      currentSchema <- withCurrentHge databaseSchema \server -> do
        Http.postValue (serverGraphqlUrl server) mempty introspectionQuery

      currentSchema `shouldBeYaml` baseSchema
  where
    baseVersion = optionsBaseVersion options

-- | The contents of /introspection.gql/, wrapped in a GraphQL JSON query.
introspectionQuery :: J.Value
introspectionQuery = J.object ["query" .= Text.decodeUtf8 rawQuery]
  where
    rawQuery = ByteString.fromStrict $(makeRelativeToProject "introspection.gql" >>= embedFile)

-- | Gets the length of @.data.__schema.types@ from an introspected schema.
--
-- We use this to ensure that the metadata looks correct.
typeLength :: forall m. (MonadFail m) => J.Value -> m Int
typeLength schema = do
  types <- getProperty "data" schema >>= getProperty "__schema" >>= getProperty "types"
  case types of
    J.Array elements -> pure $ Vector.length elements
    _ -> fail $ "Expected types to be an array, but got: " <> serialize types
  where
    getProperty :: J.Key -> J.Value -> m J.Value
    getProperty key value@(J.Object properties) =
      (J.KeyMap.lookup key properties)
        `onNothing` fail ("Could not find key " <> show key <> " in object " <> serialize value)
    getProperty _ value = fail $ "Expected an object, but got: " <> serialize value
    serialize :: J.Value -> String
    serialize value = ByteString.Char8.unpack (J.encode value)
