{-# LANGUAGE QuasiQuotes #-}

-- | This module contains functions that help defining tests in terms the
-- results of GraphQL introspection queries.
module Harness.Test.Introspection
  ( introspectTypes,
    introspectEnums,
  )
where

import Data.Aeson.Types (Parser, Value, listParser, parseEither, withObject, (.:), (.:?))
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Harness.GraphqlEngine qualified as GraphqlEngine
import Harness.Quoter.Graphql
import Harness.TestEnvironment (TestEnvironment)
import Hasura.Prelude

----------------------------------
-- Test helpers

-- | Query the names of all known types by a GraphQL introspection query against
-- a 'TestEnvironment' under a specific role.
introspectTypes :: TestEnvironment -> Text -> IO [Text]
introspectTypes env role = do
  res <-
    GraphqlEngine.postGraphqlWithHeaders
      env
      [("X-Hasura-Role", encodeUtf8 role)]
      [graphql|
          query IntrospectTypes { __schema {
            types { name }
          }}
        |]
  onLeft (parseEither getTypes res) fail
  where
    getTypes :: Value -> Parser [Text]
    getTypes = withObject "introspection top-level" $ \top -> do
      d <- top .: "data"
      sch <- d .: "__schema"
      types <- sch .: "types"
      listParser parseType types

    parseType :: Value -> Parser Text
    parseType = withObject "a 'types' element" $ \types -> types .: "name"

-- | Query all known enum types by a GraphQL introspection query against
-- a 'TestEnvironment' under a specific role.
--
-- Returns the names and enum values '(enumName :: Text, enumValues :: [Text])'
-- of all known enums.
introspectEnums :: TestEnvironment -> Text -> IO [(Text, [Text])]
introspectEnums env role = do
  res <-
    GraphqlEngine.postGraphqlWithHeaders
      env
      [("X-Hasura-Role", encodeUtf8 role)]
      [graphql|
        query IntrospectEnums {
          __schema {
            types {
              enumValues {
                name
              }
              name
            }
          }
        }
      |]
  onLeft (parseEither getEnums res) fail
  where
    getEnums :: Value -> Parser [(Text, [Text])]
    getEnums = withObject "introspection top-level" $ \top -> do
      d <- top .: "data"
      sch <- d .: "__schema"
      types <- sch .: "types"
      catMaybes <$> listParser parseEnum types

    parseEnum :: Value -> Parser (Maybe (Text, [Text]))
    parseEnum = withObject "a 'types' element" $ \types -> do
      name <- types .: "name"
      maybeVals <- types .:? "enumValues"
      case maybeVals of
        Nothing -> return Nothing
        Just vals -> Just . (name,) <$> listParser parseEnumValue vals

    parseEnumValue :: Value -> Parser Text
    parseEnumValue = withObject "enumValue" (.: "name")
