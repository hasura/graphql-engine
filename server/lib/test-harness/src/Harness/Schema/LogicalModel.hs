{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Common interface for logical models across backends
module Harness.Schema.LogicalModel
  ( LogicalModel (..),
    LogicalModelColumn (..),
    logicalModel,
    logicalModelScalar,
    logicalModelArrayReference,
    logicalModelObjectReference,
    trackLogicalModel,
    trackLogicalModelCommand,
    untrackLogicalModel,
    untrackLogicalModelCommand,
  )
where

import Data.Aeson (Value, (.=))
import Data.Aeson qualified as J
import Data.Vector qualified as V
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendTypeConfig)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.ScalarType
import Harness.TestEnvironment (TestEnvironment, getBackendTypeConfig)
import Hasura.Prelude

data ReferenceType = ArrayReference | ObjectReference
  deriving (Show, Eq)

instance J.ToJSON ReferenceType where
  toJSON ArrayReference = "array"
  toJSON ObjectReference = "object"

data LogicalModelColumn
  = LogicalModelScalar
      { logicalModelColumnName :: Text,
        logicalModelColumnType :: ScalarType,
        logicalModelColumnNullable :: Bool,
        logicalModelColumnDescription :: Maybe Text
      }
  | LogicalModelReference
      { logicalModelColumnName :: Text,
        logicalModelColumnReference :: Text,
        logicalModelColumnReferenceType :: ReferenceType
      }
  deriving (Show, Eq)

logicalModelScalar :: Text -> ScalarType -> LogicalModelColumn
logicalModelScalar name colType =
  LogicalModelScalar
    { logicalModelColumnName = name,
      logicalModelColumnType = colType,
      logicalModelColumnNullable = False,
      logicalModelColumnDescription = Nothing
    }

logicalModelArrayReference :: Text -> Text -> LogicalModelColumn
logicalModelArrayReference name ref =
  LogicalModelReference
    { logicalModelColumnName = name,
      logicalModelColumnReference = ref,
      logicalModelColumnReferenceType = ArrayReference
    }

logicalModelObjectReference :: Text -> Text -> LogicalModelColumn
logicalModelObjectReference name ref =
  LogicalModelReference
    { logicalModelColumnName = name,
      logicalModelColumnReference = ref,
      logicalModelColumnReferenceType = ObjectReference
    }

data LogicalModel = LogicalModel
  { logicalModelName :: Text,
    logicalModelColumns :: [LogicalModelColumn],
    logicalModelDescription :: Maybe Text
  }
  deriving (Show, Eq)

logicalModel :: Text -> LogicalModel
logicalModel logicalModelName =
  LogicalModel
    { logicalModelName,
      logicalModelColumns = mempty,
      logicalModelDescription = Nothing
    }

trackLogicalModelCommand :: String -> BackendTypeConfig -> LogicalModel -> Value
trackLogicalModelCommand sourceName backendTypeConfig (LogicalModel {logicalModelDescription, logicalModelName, logicalModelColumns}) =
  -- return type is an array of items
  let returnTypeToJson =
        J.Array
          . V.fromList
          . fmap
            ( \case
                LogicalModelReference {..} ->
                  J.object $
                    [ ("logical_model" .= logicalModelColumnReference),
                      ("link_type" .= logicalModelColumnReferenceType),
                      ("name" .= logicalModelColumnName)
                    ]
                LogicalModelScalar {..} ->
                  let descriptionPair = case logicalModelColumnDescription of
                        Just desc -> [("description" .= desc)]
                        Nothing -> []
                   in J.object $
                        [ ("name" .= logicalModelColumnName),
                          ("type" .= (BackendType.backendScalarType backendTypeConfig) logicalModelColumnType),
                          ("nullable" .= logicalModelColumnNullable)
                        ]
                          <> descriptionPair
            )

      columns = returnTypeToJson logicalModelColumns

      -- need to make this only appear if it's Just, for now fall back to empty
      -- string for lols
      description = fromMaybe "" logicalModelDescription

      backendType = BackendType.backendTypeString backendTypeConfig

      requestType = backendType <> "_track_logical_model"
   in [yaml|
        type: *requestType
        args:
          source: *sourceName
          description: *description 
          name: *logicalModelName
          fields: *columns
      |]

trackLogicalModel :: HasCallStack => String -> LogicalModel -> TestEnvironment -> IO ()
trackLogicalModel sourceName ctmType testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = trackLogicalModelCommand sourceName backendTypeMetadata ctmType

  GraphqlEngine.postMetadata_ testEnvironment command

untrackLogicalModelCommand :: String -> BackendTypeConfig -> LogicalModel -> Value
untrackLogicalModelCommand source backendTypeMetadata LogicalModel {logicalModelName} =
  let backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_logical_model"
   in [yaml|
      type: *requestType
      args:
        source: *source
        name: *logicalModelName
    |]

untrackLogicalModel :: HasCallStack => String -> LogicalModel -> TestEnvironment -> IO ()
untrackLogicalModel source ctmType testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = untrackLogicalModelCommand source backendTypeMetadata ctmType

  GraphqlEngine.postMetadata_
    testEnvironment
    command
