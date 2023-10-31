{-# LANGUAGE QuasiQuotes #-}

-- | Sqlite Chinook Agent Configuration
--
-- NOTE: This module is intended to be imported qualified.
module Harness.Backend.DataConnector.Chinook.Sqlite
  ( backendTypeConfig,
    mkChinookCloneTestEnvironment,
    chinookFixture,
  )
where

--------------------------------------------------------------------------------

import Control.Monad.Managed (Managed)
import Harness.Backend.DataConnector.Chinook (ChinookTestEnv, NameFormatting (..), ScalarTypes (..))
import Harness.Backend.DataConnector.Chinook qualified as Chinook
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture (Fixture (..), FixtureName (..))
import Harness.TestEnvironment
import Hasura.Prelude

--------------------------------------------------------------------------------

backendTypeConfig :: BackendType.BackendTypeConfig
backendTypeConfig =
  BackendType.BackendTypeConfig
    { backendType = BackendType.DataConnectorSqlite,
      backendSourceName = "chinook_sqlite",
      backendCapabilities =
        Just
          [yaml|
            data_schema:
              supports_primary_keys: true
              supports_foreign_keys: true
            post_schema: {}
            scalar_types:
              DateTime:
                comparison_operators:
                  _in_year: int
                graphql_type: String
              string:
                comparison_operators:
                  _like: string
                  _glob: string
                aggregate_functions:
                  min: string
                  max: string
                graphql_type: String
              decimal:
                comparison_operators:
                  _modulus_is_zero: decimal
                aggregate_functions:
                  min: decimal
                  max: decimal
                  sum: decimal
                update_column_operators:
                  inc:
                    argument_type: decimal
                  dec:
                    argument_type: decimal
                graphql_type: Float
              number:
                comparison_operators:
                  _modulus_is_zero: number
                aggregate_functions:
                  min: number
                  max: number
                  sum: number
                update_column_operators:
                  inc:
                    argument_type: number
                  dec:
                    argument_type: number
                graphql_type: Float
              bool:
                comparison_operators:
                  _and: bool
                  _or: bool
                  _nand: bool
                  _xor: bool
                graphql_type: Boolean
            queries:
              foreach: {}
            relationships: {}
            comparisons:
              subquery:
                supports_relations: true
            explain: {}
            mutations:
              atomicity_support_level: heterogeneous_operations
              delete: {}
              insert:
                supports_nested_inserts: true
              returning: {}
              update: {}
            metrics: {}
            raw: {}
        |],
      backendTypeString = "sqlite",
      backendDisplayNameString = "Hasura SQLite",
      backendReleaseNameString = Nothing,
      backendServerUrl = Just "http://localhost:65007",
      backendSchemaKeyword = "schema",
      backendScalarType = const "",
      backendGraphQLType = const ""
    }

--------------------------------------------------------------------------------

mkChinookCloneTestEnvironment :: TestEnvironment -> Managed ChinookTestEnv
mkChinookCloneTestEnvironment = Chinook.mkChinookCloneTestEnvironment nameFormatting scalarTypes

nameFormatting :: NameFormatting
nameFormatting = NameFormatting id id formatForeignKeyName

-- | Construct foreign key relationship names.
formatForeignKeyName :: Text -> Text
formatForeignKeyName = \case
  "Artist" -> "Album.ArtistId->Artist.ArtistId"
  x -> x

scalarTypes :: ScalarTypes
scalarTypes =
  ScalarTypes
    { _stFloatType = "number",
      _stIntegerType = "number",
      _stStringType = "string"
    }

chinookFixture :: Fixture ChinookTestEnv
chinookFixture =
  Fixture
    { name = Backend backendTypeConfig,
      mkLocalTestEnvironment = mkChinookCloneTestEnvironment,
      setupTeardown = \testEnvs ->
        [Chinook.setupChinookSourceAction testEnvs],
      customOptions = Nothing
    }
