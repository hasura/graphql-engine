{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Check the typechecking validation of logical model's custom return types.
module Test.API.Metadata.LogicalModels.TypeCheckingSpec where

import Data.List.NonEmpty qualified as NE
import Harness.Backend.Citus qualified as Citus
import Harness.Backend.Cockroach qualified as Cockroach
import Harness.Backend.Postgres qualified as Postgres
import Harness.Exceptions (SomeException, catch)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Schema qualified as Schema
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.Fixture qualified as Fixture
import Harness.TestEnvironment (GlobalTestEnvironment, TestEnvironment, getBackendTypeConfig)
import Harness.Yaml (shouldAtLeastBe, shouldReturnYaml)
import Hasura.Prelude
import Hasura.SQL.Backend (PostgresKind (..))
import Test.Hspec (SpecWith, describe, it)
import Test.QuickCheck

featureFlagForLogicalModels :: String
featureFlagForLogicalModels = "HASURA_FF_LOGICAL_MODEL_INTERFACE"

spec :: SpecWith GlobalTestEnvironment
spec = do
  Fixture.hgeWithEnv [(featureFlagForLogicalModels, "True")] do
    Fixture.run
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Postgres.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Postgres.setupTablesAction schema testEnv
                  ]
              },
            (Fixture.fixture $ Fixture.Backend Citus.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Citus.setupTablesAction schema testEnv
                  ]
              }
          ]
      )
      (tests @'Vanilla)
    Fixture.run
      ( NE.fromList
          [ (Fixture.fixture $ Fixture.Backend Cockroach.backendTypeMetadata)
              { Fixture.setupTeardown = \(testEnv, _) ->
                  [ Cockroach.setupTablesAction schema testEnv
                  ]
              }
          ]
      )
      (tests @'Cockroach)

-- ** Setup and teardown

customType :: Text -> Schema.ScalarType
customType txt =
  Schema.TCustomType
    Schema.defaultBackendScalarType
      { Schema.bstPostgres = Just txt,
        Schema.bstCitus = Just txt,
        Schema.bstCockroach = Just txt
      }

schema :: [Schema.Table]
schema =
  [ (Schema.table "stuff")
      { Schema.tableColumns =
          (\t -> Schema.column t (customType t)) <$> types
      }
  ]
    <> fmap
      ( \t ->
          (Schema.table ("stuff_" <> t))
            { Schema.tableColumns =
                [Schema.column t (customType t)]
            }
      )
      types

allTypesReturnType :: Schema.CustomType
allTypesReturnType =
  (Schema.customType "stuff_type")
    { Schema.customTypeColumns =
        (\t -> Schema.logicalModelColumn t (customType t)) <$> types
    }

types :: [Text]
types =
  [ "int2",
    "smallint",
    "integer",
    "bigint",
    "int8",
    "real",
    "float8",
    "numeric",
    "bool",
    "char",
    "varchar",
    "text",
    "date",
    "timestamp",
    "timestamptz",
    "timetz",
    "json",
    "jsonb",
    "uuid"
  ]

-- ** Tests

tests :: forall pgKind. GetDiffs pgKind => SpecWith TestEnvironment
tests = do
  describe "Validation succeeds tracking a logical model" do
    it "for all supported types" $
      \testEnvironment -> do
        let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
            sourceName = BackendType.backendSourceName backendTypeMetadata

        let simpleQuery :: Text
            simpleQuery = "SELECT * FROM stuff"

        let logicalModel :: Schema.LogicalModel
            logicalModel =
              (Schema.logicalModel "typed_model" simpleQuery "stuff_type")

        Schema.trackCustomType sourceName allTypesReturnType testEnvironment

        shouldReturnYaml
          testEnvironment
          ( GraphqlEngine.postMetadata
              testEnvironment
              (Schema.trackLogicalModelCommand sourceName backendTypeMetadata logicalModel)
          )
          [yaml|
          message: success
        |]

  describe "Validation fails tracking a logical model" do
    it "when there's a type mismatch" $ \testEnvironment -> withMaxSuccess (maxSuccesses @pgKind) $ \(TypeForQuickCheck {..} :: TypeForQuickCheck pgKind) -> do
      let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment
          sourceName = BackendType.backendSourceName backendTypeMetadata

      let wrongQuery :: Text
          wrongQuery = "SELECT " <> tableType <> " AS " <> customtypeType <> " FROM stuff_" <> tableType

      let logicalModel :: Schema.LogicalModel
          logicalModel =
            (Schema.logicalModel ("typed_model_" <> customtypeType) wrongQuery ("stuff_type_" <> customtypeType))

      -- Possible cleanup after last test that may have tracked this custom type
      _ <- Schema.untrackLogicalModel sourceName logicalModel testEnvironment `catch` \(_ :: SomeException) -> pure ()
      _ <- Schema.untrackCustomType sourceName (mkCustomType customtypeType) testEnvironment `catch` \(_ :: SomeException) -> pure ()
      Schema.trackCustomType sourceName (mkCustomType customtypeType) testEnvironment

      let message :: Text
          message =
            "Return column '"
              <> customtypeType
              <> "' has a type mismatch. The expected type is '"
              <> customTypeNameMapping @pgKind customtypeType
              <> "', but the actual type is '"
              <> tableTypeNameMapping @pgKind tableType
              <> "'."
          expected =
            [yaml|
                    code: validation-failed
                    error: Failed to validate query
                    internal: *message
                |]

      actual <-
        GraphqlEngine.postMetadataWithStatus
          400
          testEnvironment
          (Schema.trackLogicalModelCommand sourceName backendTypeMetadata logicalModel)
      actual `shouldAtLeastBe` expected

-- ** Utils

mkCustomType :: Text -> Schema.CustomType
mkCustomType typ =
  (Schema.customType ("stuff_type_" <> typ))
    { Schema.customTypeColumns =
        [Schema.logicalModelColumn typ (customType typ)]
    }

isDifferentTypeThanPg :: Text -> Text -> Bool
isDifferentTypeThanPg a b
  | a == b = False
  | ["int2", "smallint"] == sort [a, b] = False
  | ["bigint", "int8"] == sort [a, b] = False
  | otherwise = True

isDifferentTypeThanRoach :: Text -> Text -> Bool
isDifferentTypeThanRoach a b
  | a == b = False
  | sort ["smallint", "int2"] == sort [a, b] = False
  | sort ["integer", "int8"] == sort [a, b] = False
  | sort ["bigint", "int8"] == sort [a, b] = False
  | sort ["bigint", "integer"] == sort [a, b] = False
  | sort ["json", "jsonb"] == sort [a, b] = False
  | otherwise = True

data TypeForQuickCheck pgKind = TypeForQuickCheck {tableType :: Text, customtypeType :: Text}
  deriving (Show)

instance GetDiffs pgKind => Arbitrary (TypeForQuickCheck pgKind) where
  shrink = const []
  arbitrary =
    uncurry TypeForQuickCheck
      <$> suchThat ((,) <$> elements types <*> elements types) (uncurry (isDifferentTypeThan @pgKind))

class GetDiffs (pgKind :: PostgresKind) where
  maxSuccesses :: Int
  maxSuccesses = 100
  isDifferentTypeThan :: Text -> Text -> Bool
  isDifferentTypeThan = isDifferentTypeThanPg
  customTypeNameMapping :: Text -> Text
  customTypeNameMapping = tableTypeNameMapping @pgKind
  tableTypeNameMapping :: Text -> Text
  tableTypeNameMapping = \case
    "bool" -> "boolean"
    "char" -> "bpchar"
    "int2" -> "smallint"
    "int8" -> "bigint"
    t -> t

instance GetDiffs 'Vanilla

instance GetDiffs 'Citus

instance GetDiffs 'Cockroach where
  maxSuccesses = 30
  isDifferentTypeThan = isDifferentTypeThanRoach
  customTypeNameMapping = \case
    "bool" -> "boolean"
    "char" -> "bpchar"
    "int2" -> "smallint"
    "int8" -> "bigint"
    t -> t
  tableTypeNameMapping = \case
    "bool" -> "boolean"
    "char" -> "bpchar"
    "int2" -> "smallint"
    "integer" -> "bigint"
    "int8" -> "bigint"
    "json" -> "jsonb"
    t -> t
