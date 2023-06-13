// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict'

import { afterAll, beforeAll, expect, test } from '@jest/globals'
import { GraphQLObjectType, GraphQLSchema } from 'graphql'

import * as openAPIToGraphQL from '../src/index'

// Set up the schema first
const oas = require('./fixtures/ibm_language_translator.json')

let createdSchema: GraphQLSchema
beforeAll(() => {
  return openAPIToGraphQL
    .createGraphQLSchema(oas)
    .then(({ schema, report }) => {
      createdSchema = schema
    })
})

test('All IBM Language Translator query endpoints present', () => {
  let oasGetCount = 0
  for (let path in oas.paths) {
    for (let method in oas.paths[path]) {
      if (method === 'get') oasGetCount++
    }
  }
  const gqlTypes = Object.keys(
    ((createdSchema.getTypeMap().Query as GraphQLObjectType).getFields().viewerAnyAuth.type as GraphQLObjectType).getFields()
  ).length

  expect(gqlTypes).toEqual(oasGetCount)
})
