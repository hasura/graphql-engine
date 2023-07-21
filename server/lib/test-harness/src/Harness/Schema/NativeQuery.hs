{-# LANGUAGE QuasiQuotes #-}

module Harness.Schema.NativeQuery
  ( NativeQuery (..),
    NativeQueryColumn (..),
    trackNativeQueryCommand,
    untrackNativeQueryCommand,
    trackNativeQuery,
    untrackNativeQuery,
    nativeQuery,
    nativeQueryColumn,
    inlineNativeQuery,
  )
where

import Data.Aeson (Value, (.=))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Vector qualified as V
import Harness.Exceptions
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Yaml (yaml)
import Harness.Schema.LogicalModel
import Harness.Schema.Table
import Harness.Test.BackendType (BackendTypeConfig)
import Harness.Test.BackendType qualified as BackendType
import Harness.TestEnvironment (TestEnvironment (..), getBackendTypeConfig)
import Hasura.Prelude

data NativeQueryColumn = NativeQueryColumn
  { nativeQueryColumnName :: Text,
    nativeQueryColumnType :: ScalarType,
    nativeQueryColumnNullable :: Bool,
    nativeQueryColumnDescription :: Maybe Text
  }
  deriving (Show, Eq)

nativeQueryColumn :: Text -> ScalarType -> NativeQueryColumn
nativeQueryColumn name colType =
  NativeQueryColumn
    { nativeQueryColumnName = name,
      nativeQueryColumnType = colType,
      nativeQueryColumnNullable = False,
      nativeQueryColumnDescription = Nothing
    }

data NativeQueryReturn
  = NQNamedLogicalModel Text
  | NQFields [LogicalModelColumn]

data NativeQuery = NativeQuery
  { nativeQueryName :: Text,
    nativeQueryReturn :: NativeQueryReturn,
    nativeQueryQuery :: BackendType.BackendType -> Text,
    nativeQueryArguments :: [NativeQueryColumn],
    nativeQueryArrayRelationships :: [J.Value],
    nativeQueryObjectRelationships :: [J.Value]
  }

-- | A native query with a named Logical Model
nativeQuery :: Text -> (BackendType.BackendType -> Text) -> Text -> NativeQuery
nativeQuery nativeQueryName query returnType =
  NativeQuery
    { nativeQueryName,
      nativeQueryReturn = NQNamedLogicalModel returnType,
      nativeQueryQuery = query,
      nativeQueryArguments = mempty,
      nativeQueryArrayRelationships = mempty,
      nativeQueryObjectRelationships = mempty
    }

-- | A natiev query with an inline Logical Model
inlineNativeQuery :: Text -> (BackendType.BackendType -> Text) -> [LogicalModelColumn] -> NativeQuery
inlineNativeQuery nativeQueryName query fields =
  NativeQuery
    { nativeQueryName,
      nativeQueryReturn = NQFields fields,
      nativeQueryQuery = query,
      nativeQueryArguments = mempty,
      nativeQueryArrayRelationships = mempty,
      nativeQueryObjectRelationships = mempty
    }

trackNativeQueryCommand :: String -> BackendTypeConfig -> NativeQuery -> Value
trackNativeQueryCommand sourceName backendTypeConfig (NativeQuery {nativeQueryObjectRelationships, nativeQueryArrayRelationships, nativeQueryName, nativeQueryArguments, nativeQueryQuery, nativeQueryReturn}) =
  -- arguments are a map from name to type details
  let argsToJson =
        J.object
          . fmap
            ( \NativeQueryColumn {..} ->
                let key = K.fromText nativeQueryColumnName
                    descriptionPair = case nativeQueryColumnDescription of
                      Just desc -> ["description" .= desc]
                      Nothing -> []

                    value =
                      J.object
                        $ [ ("type" .= (BackendType.backendScalarType backendTypeConfig) nativeQueryColumnType),
                            ("nullable" .= nativeQueryColumnNullable)
                          ]
                        <> descriptionPair
                 in (key, value)
            )

      arguments = argsToJson nativeQueryArguments

      requestType = BackendType.backendTypeString backendTypeConfig <> "_track_native_query"

      query = nativeQueryQuery (BackendType.backendType backendTypeConfig)

      returns = case nativeQueryReturn of
        NQNamedLogicalModel lm -> J.String lm
        NQFields fields ->
          J.object
            $ [ ( "fields",
                  J.Array
                    . V.fromList
                    . fmap (logicalModelColumnToJSON backendTypeConfig)
                    $ fields
                )
              ]
   in [yaml|
        type: *requestType
        args:
          type: query
          source: *sourceName
          root_field_name: *nativeQueryName
          code: *query
          arguments: *arguments
          array_relationships: *nativeQueryArrayRelationships
          object_relationships: *nativeQueryObjectRelationships
          returns: *returns
      |]

trackNativeQuery :: (HasCallStack) => String -> NativeQuery -> TestEnvironment -> IO ()
trackNativeQuery sourceName naqu testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = trackNativeQueryCommand sourceName backendTypeMetadata naqu

  GraphqlEngine.postMetadata_ testEnvironment command

untrackNativeQueryCommand :: String -> BackendTypeConfig -> NativeQuery -> Value
untrackNativeQueryCommand source backendTypeMetadata NativeQuery {nativeQueryName} =
  let backendType = BackendType.backendTypeString backendTypeMetadata
      requestType = backendType <> "_untrack_native_query"
   in [yaml|
      type: *requestType
      args:
        source: *source
        root_field_name: *nativeQueryName
    |]

untrackNativeQuery :: (HasCallStack) => String -> NativeQuery -> TestEnvironment -> IO ()
untrackNativeQuery source logMod testEnvironment = do
  let backendTypeMetadata = fromMaybe (error "Unknown backend") $ getBackendTypeConfig testEnvironment

  let command = untrackNativeQueryCommand source backendTypeMetadata logMod

  GraphqlEngine.postMetadata_
    testEnvironment
    command
