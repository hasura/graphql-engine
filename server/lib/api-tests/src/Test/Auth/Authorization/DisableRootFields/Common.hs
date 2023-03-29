{-# LANGUAGE QuasiQuotes #-}

-- | Common functions for test
module Test.Auth.Authorization.DisableRootFields.Common
  ( listQuery,
    listRFEnabledExpectedResponse,
    listRFDisabledExpectedResponse,
    pkQuery,
    pkRFEnabledExpectedResponse,
    pkRFDisabledExpectedResponse,
    aggregateQuery,
    aggRFEnabledExpectedResponse,
    aggRFDisabledExpectedResponse,
    queryTypesIntrospection,
    subscriptionTypesIntrospection,
  )
where

import Data.Aeson (Value)
import Harness.Quoter.Graphql (graphql)
import Harness.Quoter.Yaml (interpolateYaml)
import Harness.Schema qualified as Schema
import Harness.TestEnvironment (TestEnvironment)

listQuery :: TestEnvironment -> Value
listQuery testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

  [graphql|
    query {
      #{schemaName}_author {
        id
        name
      }
    }
  |]

listRFEnabledExpectedResponse :: TestEnvironment -> Value
listRFEnabledExpectedResponse testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

  [interpolateYaml|
    data:
      #{schemaName}_author:
        - id: 1
          name: Author 1
  |]

listRFDisabledExpectedResponse :: TestEnvironment -> Value
listRFDisabledExpectedResponse testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

  [interpolateYaml|
    errors:
      - extensions:
          path: $.selectionSet.#{schemaName}_author
          code: validation-failed
        message: |-
          field '#{schemaName}_author' not found in type: 'query_root'
  |]

pkQuery :: TestEnvironment -> Value
pkQuery testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

  [graphql|
    query {
      #{schemaName}_author_by_pk(id: 1) {
        id
        name
      }
    }
  |]

pkRFEnabledExpectedResponse :: TestEnvironment -> Value
pkRFEnabledExpectedResponse testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

  [interpolateYaml|
    data:
      #{schemaName}_author_by_pk:
        id: 1
        name: Author 1
  |]

pkRFDisabledExpectedResponse :: TestEnvironment -> Value
pkRFDisabledExpectedResponse testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

  [interpolateYaml|
    errors:
      - extensions:
          path: $.selectionSet.#{schemaName}_author_by_pk
          code: validation-failed
        message: |-
          field '#{schemaName}_author_by_pk' not found in type: 'query_root'
  |]

aggregateQuery :: TestEnvironment -> Value
aggregateQuery testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

  [graphql|
    query {
      #{schemaName}_author_aggregate {
        aggregate {
          count
        }
      }
    }
  |]

aggRFEnabledExpectedResponse :: TestEnvironment -> Value
aggRFEnabledExpectedResponse testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

  [interpolateYaml|
    data:
      #{schemaName}_author_aggregate:
        aggregate:
          count: 1
  |]

aggRFDisabledExpectedResponse :: TestEnvironment -> Value
aggRFDisabledExpectedResponse testEnvironment = do
  let schemaName :: Schema.SchemaName
      schemaName = Schema.getSchemaName testEnvironment

  [interpolateYaml|
    errors:
      - extensions:
          path: $.selectionSet.#{schemaName}_author_aggregate
          code: validation-failed
        message: |-
          field '#{schemaName}_author_aggregate' not found in type: 'query_root'
  |]

queryTypesIntrospection :: Value
queryTypesIntrospection =
  [graphql|
    query {
      __schema {
        queryType {
          fields{
            name
          }
        }
      }
    }
  |]

subscriptionTypesIntrospection :: Value
subscriptionTypesIntrospection =
  [graphql|
    query {
      __schema {
        subscriptionType {
          fields{
            name
          }

        }
      }
    }
  |]
