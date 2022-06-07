{-# LANGUAGE QuasiQuotes #-}

-- | Common functions for test
module Test.DisableRootFields.Common
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
import Harness.Quoter.Yaml (yaml)

listQuery :: Value
listQuery =
  [graphql|
  query {
    hasura_author {
      id
      name
    }
  }
  |]

listRFEnabledExpectedResponse :: Value
listRFEnabledExpectedResponse =
  [yaml|
  data:
    hasura_author:
      - id: 1
        name: Author 1
  |]

listRFDisabledExpectedResponse :: Value
listRFDisabledExpectedResponse =
  [yaml|
  errors:
    - extensions:
        path: $.selectionSet.hasura_author
        code: validation-failed
      message: 'field "hasura_author" not found in type: ''query_root'''
  |]

pkQuery :: Value
pkQuery =
  [graphql|
  query {
    hasura_author_by_pk(id: 1) {
      id
      name
    }
  }
  |]

pkRFEnabledExpectedResponse :: Value
pkRFEnabledExpectedResponse =
  [yaml|
  data:
    hasura_author_by_pk:
      id: 1
      name: Author 1
  |]

pkRFDisabledExpectedResponse :: Value
pkRFDisabledExpectedResponse =
  [yaml|
  errors:
    - extensions:
        path: $.selectionSet.hasura_author_by_pk
        code: validation-failed
      message: 'field "hasura_author_by_pk" not found in type: ''query_root'''
  |]

aggregateQuery :: Value
aggregateQuery =
  [graphql|
  query {
    hasura_author_aggregate {
      aggregate {
        count
      }
    }
  }
  |]

aggRFEnabledExpectedResponse :: Value
aggRFEnabledExpectedResponse =
  [yaml|
  data:
    hasura_author_aggregate:
      aggregate:
        count: 1
  |]

aggRFDisabledExpectedResponse :: Value
aggRFDisabledExpectedResponse =
  [yaml|
  errors:
    - extensions:
        path: $.selectionSet.hasura_author_aggregate
        code: validation-failed
      message: 'field "hasura_author_aggregate" not found in type: ''query_root'''
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
