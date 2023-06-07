{-# LANGUAGE QuasiQuotes #-}

module Harness.Schema.StoredProcedure
  ( StoredProcedure (..),
    StoredProcedureColumn (..),
    trackStoredProcedureCommand,
    untrackStoredProcedureCommand,
    trackStoredProcedure,
    untrackStoredProcedure,
    storedProcedure,
    storedProcedureColumn,
  )
where

import Data.Aeson (Value, (.=))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Schema.Table
import Harness.Test.BackendType (BackendTypeConfig)
import Harness.Test.BackendType qualified as BackendType
import Harness.TestEnvironment (TestEnvironment (..), getBackendTypeConfig)
import Hasura.Prelude

data StoredProcedureColumn = StoredProcedureColumn
  { storedProcedureColumnName :: Text,
    storedProcedureColumnType :: ScalarType,
    storedProcedureColumnNullable :: Bool,
    storedProcedureColumnDescription :: Maybe Text
  }
  deriving (Show, Eq)

storedProcedureColumn :: Text -> ScalarType -> StoredProcedureColumn
storedProcedureColumn name colType =
  StoredProcedureColumn
    { storedProcedureColumnName = name,
      storedProcedureColumnType = colType,
      storedProcedureColumnNullable = False,
      storedProcedureColumnDescription = Nothing
    }

data StoredProcedure = StoredProcedure
  { storedProcedureName :: Text,
    storedProcedureLogicalModel :: Text,
    storedProcedureArguments :: [StoredProcedureColumn],
    storedProcedureArrayRelationships :: [J.Value]
  }
  deriving (Show, Eq)

storedProcedure :: Text -> Text -> StoredProcedure
storedProcedure storedProcedureName returnType =
  StoredProcedure
    { storedProcedureName,
      storedProcedureLogicalModel = returnType,
      storedProcedureArguments = mempty,
      storedProcedureArrayRelationships = mempty
    }

trackStoredProcedureCommand :: String -> BackendTypeConfig -> StoredProcedure -> Value
trackStoredProcedureCommand sourceName backendTypeConfig StoredProcedure {storedProcedureArrayRelationships, storedProcedureName, storedProcedureArguments, storedProcedureLogicalModel} =
  -- arguments are a map from name to type details
  let argsToJson =
        J.object
          . fmap
            ( \StoredProcedureColumn {..} ->
                let key = K.fromText storedProcedureColumnName
                    descriptionPair = case storedProcedureColumnDescription of
                      Just desc -> ["description" .= desc]
                      Nothing -> []

                    value =
                      J.object
                        $ [ ("type" .= (BackendType.backendScalarType backendTypeConfig) storedProcedureColumnType),
                            ("nullable" .= storedProcedureColumnNullable)
                          ]
                        <> descriptionPair
                 in (key, value)
            )

      arguments = argsToJson storedProcedureArguments

      backendType = BackendType.backendTypeString backendTypeConfig

      requestType = backendType <> "_track_stored_procedure"
   in [yaml|
        type: *requestType
        args:
          type: query
          source: *sourceName
          stored_procedure: *storedProcedureName
          configuration:
            exposed_as: query
          arguments: *arguments
          array_relationships: *storedProcedureArrayRelationships
          returns: *storedProcedureLogicalModel
      |]

trackStoredProcedure :: (HasCallStack) => String -> StoredProcedure -> TestEnvironment -> IO ()
trackStoredProcedure sourceName logMod testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = trackStoredProcedureCommand sourceName backendTypeMetadata logMod

  GraphqlEngine.postMetadata_ testEnvironment command

untrackStoredProcedureCommand :: String -> BackendTypeConfig -> StoredProcedure -> Value
untrackStoredProcedureCommand source backendTypeMetadata StoredProcedure {storedProcedureName} =
  let backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_stored_procedure"
   in [yaml|
      type: *requestType
      args:
        source: *source
        stored_procedure: *storedProcedureName
    |]

untrackStoredProcedure :: (HasCallStack) => String -> StoredProcedure -> TestEnvironment -> IO ()
untrackStoredProcedure source logMod testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = untrackStoredProcedureCommand source backendTypeMetadata logMod

  GraphqlEngine.postMetadata_
    testEnvironment
    command
