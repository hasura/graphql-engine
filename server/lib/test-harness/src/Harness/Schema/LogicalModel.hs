{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Common interface for logical models across backends
module Harness.Schema.LogicalModel
  ( LogicalModel (..),
    LogicalModelColumn (..),
    logicalModel,
    logicalModelScalar,
    logicalModelArrayScalar,
    logicalModelArrayReference,
    logicalModelObjectReference,
    trackLogicalModel,
    trackLogicalModelCommand,
    untrackLogicalModel,
    untrackLogicalModelCommand,
    logicalModelColumnToJSON,
    makeNullable,
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

data LogicalModelColumn = LogicalModelColumn
  { logicalModelColumnName :: Text,
    logicalModelColumnDescription :: Maybe Text,
    logicalModelColumnType :: LogicalModelType
  }
  deriving (Show, Eq)

data LogicalModelType
  = LogicalModelScalar
      { logicalModelTypeScalar :: ScalarType,
        logicalModelTypeNullable :: Bool
      }
  | LogicalModelReference
      { logicalModelTypeReference :: Text,
        logicalModelTypeNullable :: Bool
      }
  | LogicalModelArray
      { logicalModelTypeArray :: LogicalModelType,
        logicalModelTypeNullable :: Bool
      }
  deriving (Show, Eq)

makeNullable :: LogicalModelColumn -> LogicalModelColumn
makeNullable (LogicalModelColumn {logicalModelColumnName, logicalModelColumnDescription, logicalModelColumnType}) =
  LogicalModelColumn
    { logicalModelColumnName,
      logicalModelColumnDescription,
      logicalModelColumnType = case logicalModelColumnType of
        LogicalModelScalar scalar _ -> LogicalModelScalar scalar True
        LogicalModelReference ref _ -> LogicalModelReference ref True
        LogicalModelArray arr _ -> LogicalModelArray arr True
    }

logicalModelScalar :: Text -> ScalarType -> LogicalModelColumn
logicalModelScalar name colType =
  LogicalModelColumn
    { logicalModelColumnName = name,
      logicalModelColumnDescription = Nothing,
      logicalModelColumnType =
        LogicalModelScalar
          { logicalModelTypeScalar = colType,
            logicalModelTypeNullable = False
          }
    }

logicalModelArrayScalar :: Text -> ScalarType -> LogicalModelColumn
logicalModelArrayScalar name colType =
  LogicalModelColumn
    { logicalModelColumnName = name,
      logicalModelColumnDescription = Nothing,
      logicalModelColumnType =
        LogicalModelArray
          { logicalModelTypeArray =
              LogicalModelScalar
                { logicalModelTypeScalar = colType,
                  logicalModelTypeNullable = False
                },
            logicalModelTypeNullable = False
          }
    }

logicalModelArrayReference :: Text -> Text -> LogicalModelColumn
logicalModelArrayReference name ref =
  LogicalModelColumn
    { logicalModelColumnName = name,
      logicalModelColumnDescription = Nothing,
      logicalModelColumnType =
        LogicalModelArray
          { logicalModelTypeNullable = False,
            logicalModelTypeArray =
              LogicalModelReference
                { logicalModelTypeReference = ref,
                  logicalModelTypeNullable = False
                }
          }
    }

logicalModelObjectReference :: Text -> Text -> LogicalModelColumn
logicalModelObjectReference name ref =
  LogicalModelColumn
    { logicalModelColumnName = name,
      logicalModelColumnDescription = Nothing,
      logicalModelColumnType =
        LogicalModelReference
          { logicalModelTypeReference = ref,
            logicalModelTypeNullable = False
          }
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

logicalModelColumnToJSON :: BackendTypeConfig -> LogicalModelColumn -> Value
logicalModelColumnToJSON backendTypeConfig (LogicalModelColumn {..}) =
  let descriptionPair = case logicalModelColumnDescription of
        Just desc -> [("description" .= desc)]
        Nothing -> []
   in J.object
        $ [ ("name" .= logicalModelColumnName),
            ("type" .= logicalModelTypeToJSON backendTypeConfig logicalModelColumnType)
          ]
        <> descriptionPair

logicalModelTypeToJSON :: BackendTypeConfig -> LogicalModelType -> Value
logicalModelTypeToJSON backendTypeConfig =
  ( \case
      LogicalModelReference
        { logicalModelTypeReference,
          logicalModelTypeNullable
        } ->
          J.object
            [ "logical_model" .= logicalModelTypeReference,
              "nullable" .= logicalModelTypeNullable
            ]
      LogicalModelArray
        { logicalModelTypeArray,
          logicalModelTypeNullable
        } ->
          J.object
            $ [ ("nullable" .= logicalModelTypeNullable),
                ("array", logicalModelTypeToJSON backendTypeConfig logicalModelTypeArray)
              ]
      LogicalModelScalar {logicalModelTypeScalar, logicalModelTypeNullable} ->
        J.object
          $ [ ("scalar" .= (BackendType.backendScalarType backendTypeConfig) logicalModelTypeScalar),
              ("nullable" .= logicalModelTypeNullable)
            ]
  )

trackLogicalModelCommand :: String -> BackendTypeConfig -> LogicalModel -> Value
trackLogicalModelCommand sourceName backendTypeConfig (LogicalModel {logicalModelDescription, logicalModelName, logicalModelColumns}) =
  let columns =
        J.Array
          . V.fromList
          . fmap (logicalModelColumnToJSON backendTypeConfig)
          $ logicalModelColumns

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

trackLogicalModel :: (HasCallStack) => String -> LogicalModel -> TestEnvironment -> IO ()
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

untrackLogicalModel :: (HasCallStack) => String -> LogicalModel -> TestEnvironment -> IO ()
untrackLogicalModel source ctmType testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = untrackLogicalModelCommand source backendTypeMetadata ctmType

  GraphqlEngine.postMetadata_
    testEnvironment
    command
