-- | This module defines `SchemaName`, for naming DB schemas/datasets used in
-- tests
module Harness.Test.SchemaName (SchemaName (..), getSchemaName) where

import Data.Aeson (ToJSON (..))
import Data.Char qualified
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID
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
getSchemaName testEnv = case backendType testEnv of
  Nothing -> SchemaName "hasura" -- the `Nothing` case is for tests with multiple schemas
  Just db -> case db of
    Postgres -> SchemaName $ T.pack Constants.postgresDb
    MySQL -> SchemaName $ T.pack Constants.mysqlDb
    SQLServer -> SchemaName $ T.pack Constants.sqlserverDb
    BigQuery ->
      SchemaName $
        T.pack $
          "hasura_test_"
            <> showUUID (uniqueTestId testEnv)
    Citus -> SchemaName $ T.pack Constants.citusDb
    DataConnector -> SchemaName $ T.pack Constants.dataConnectorDb

-- | Sanitise UUID for use in BigQuery dataset name
-- must be alphanumeric (plus underscores)
showUUID :: UUID -> String
showUUID =
  map
    ( \a ->
        if Data.Char.isAlphaNum a
          then a
          else '_'
    )
    . show
