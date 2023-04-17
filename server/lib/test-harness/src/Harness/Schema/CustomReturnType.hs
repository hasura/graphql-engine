{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Common interface for custom return types across backends
module Harness.Schema.CustomReturnType
  ( CustomReturnType (..),
    CustomReturnTypeColumn (..),
    customType,
    customReturnTypeScalar,
    customReturnTypeReference,
    trackCustomReturnType,
    trackCustomReturnTypeCommand,
    untrackCustomReturnType,
    untrackCustomReturnTypeCommand,
  )
where

import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Vector qualified as V
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Test.BackendType (BackendTypeConfig)
import Harness.Test.BackendType qualified as BackendType
import Harness.Test.ScalarType
import Harness.TestEnvironment (TestEnvironment, getBackendTypeConfig)
import Hasura.Prelude

data CustomReturnTypeColumn
  = CustomReturnTypeScalar
      { customReturnTypeColumnName :: Text,
        customReturnTypeColumnType :: ScalarType,
        customReturnTypeColumnNullable :: Bool,
        customReturnTypeColumnDescription :: Maybe Text
      }
  | CustomReturnTypeReference
      { customReturnTypeColumnName :: Text,
        customReturnTypeColumnReference :: Text
      }
  deriving (Show, Eq)

customReturnTypeScalar :: Text -> ScalarType -> CustomReturnTypeColumn
customReturnTypeScalar name colType =
  CustomReturnTypeScalar
    { customReturnTypeColumnName = name,
      customReturnTypeColumnType = colType,
      customReturnTypeColumnNullable = False,
      customReturnTypeColumnDescription = Nothing
    }

customReturnTypeReference :: Text -> Text -> CustomReturnTypeColumn
customReturnTypeReference name ref =
  CustomReturnTypeReference
    { customReturnTypeColumnName = name,
      customReturnTypeColumnReference = ref
    }

data CustomReturnType = CustomReturnType
  { customTypeName :: Text,
    customTypeColumns :: [CustomReturnTypeColumn],
    customTypeDescription :: Maybe Text
  }
  deriving (Show, Eq)

customType :: Text -> CustomReturnType
customType customTypeName =
  CustomReturnType
    { customTypeName,
      customTypeColumns = mempty,
      customTypeDescription = Nothing
    }

trackCustomReturnTypeCommand :: String -> BackendTypeConfig -> CustomReturnType -> Value
trackCustomReturnTypeCommand sourceName backendTypeConfig (CustomReturnType {customTypeDescription, customTypeName, customTypeColumns}) =
  -- return type is an array of items
  let returnTypeToJson =
        Aeson.Array
          . V.fromList
          . fmap
            ( \case
                CustomReturnTypeReference {..} ->
                  Aeson.object $
                    [ ("custom_return_type" .= customReturnTypeColumnReference),
                      ("name" .= customReturnTypeColumnName)
                    ]
                CustomReturnTypeScalar {..} ->
                  let descriptionPair = case customReturnTypeColumnDescription of
                        Just desc -> [("description" .= desc)]
                        Nothing -> []
                   in Aeson.object $
                        [ ("name" .= customReturnTypeColumnName),
                          ("type" .= (BackendType.backendScalarType backendTypeConfig) customReturnTypeColumnType),
                          ("nullable" .= customReturnTypeColumnNullable)
                        ]
                          <> descriptionPair
            )

      columns = returnTypeToJson customTypeColumns

      -- need to make this only appear if it's Just, for now fall back to empty
      -- string for lols
      description = fromMaybe "" customTypeDescription

      backendType = BackendType.backendTypeString backendTypeConfig

      requestType = backendType <> "_track_custom_return_type"
   in [yaml|
        type: *requestType
        args:
          source: *sourceName
          description: *description 
          name: *customTypeName
          fields: *columns
      |]

trackCustomReturnType :: HasCallStack => String -> CustomReturnType -> TestEnvironment -> IO ()
trackCustomReturnType sourceName ctmType testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = trackCustomReturnTypeCommand sourceName backendTypeMetadata ctmType

  GraphqlEngine.postMetadata_ testEnvironment command

untrackCustomReturnTypeCommand :: String -> BackendTypeConfig -> CustomReturnType -> Value
untrackCustomReturnTypeCommand source backendTypeMetadata CustomReturnType {customTypeName} =
  let backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_custom_return_type"
   in [yaml|
      type: *requestType
      args:
        source: *source
        name: *customTypeName
    |]

untrackCustomReturnType :: HasCallStack => String -> CustomReturnType -> TestEnvironment -> IO ()
untrackCustomReturnType source ctmType testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = untrackCustomReturnTypeCommand source backendTypeMetadata ctmType

  GraphqlEngine.postMetadata_
    testEnvironment
    command
