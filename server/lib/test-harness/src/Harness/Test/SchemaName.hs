-- | This module defines `SchemaName`, for naming DB schemas/datasets used in
-- tests
module Harness.Test.SchemaName (SchemaName (..), getSchemaName) where

import Data.Aeson (ToJSON (..))
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Harness.Constants qualified as Constants
import Harness.Quoter.Graphql
import Harness.Quoter.Yaml
import Harness.Test.BackendType
import Harness.TestEnvironment
import Prelude

newtype SchemaName = SchemaName {unSchemaName :: Text}
  deriving newtype (Semigroup)

instance ToJSON SchemaName where
  toJSON (SchemaName sn) = toJSON sn

instance IsString SchemaName where
  fromString s = SchemaName (T.pack s)

instance ToGraphqlString SchemaName where
  showGql (SchemaName sn) = T.unpack sn

instance ToYamlString SchemaName where
  showYml (SchemaName sn) = T.unpack sn

-- | Given a `TestEnvironment`, returns a `SchemaName` to use in the test, used
-- to separate out different test suites
--
-- This is used both in setup and teardown, and in individual tests
--
-- The `TestEnvironment` contains a `uniqueTestId` and `backendType`, from
-- which we decide what the `SchemaName` should be.
--
-- The backendType is only required so we make changes for BigQuery for now,
-- once we do this for all backends we'll just need the unique id.
--
-- For all other backends, we fall back to the Constants that were used before
getSchemaName :: TestEnvironment -> SchemaName
getSchemaName testEnv = getSchemaNameInternal (backendType testEnv) (uniqueTestId testEnv)

-- | exposed for use when creating a TestEnvironment
getSchemaNameInternal :: Maybe BackendType -> UniqueTestId -> SchemaName
getSchemaNameInternal Nothing _ = SchemaName "hasura" -- the `Nothing` case is for tests with multiple schemas
getSchemaNameInternal (Just BigQuery) uniqueTestId =
  SchemaName $
    T.pack $
      "hasura_test_"
        <> show uniqueTestId
getSchemaNameInternal (Just Postgres) _ = SchemaName $ T.pack Constants.postgresDb
getSchemaNameInternal (Just SQLServer) _ = SchemaName $ T.pack Constants.sqlserverDb
getSchemaNameInternal (Just Citus) _ = SchemaName $ T.pack Constants.citusDb
getSchemaNameInternal (Just Cockroach) _ = SchemaName $ T.pack Constants.cockroachDb
getSchemaNameInternal (Just (DataConnector "sqlite")) _ = SchemaName "main"
getSchemaNameInternal (Just (DataConnector _)) _ = SchemaName $ T.pack Constants.dataConnectorDb
