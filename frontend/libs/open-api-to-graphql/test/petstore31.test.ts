// Copyright IBM Corp. 2017,2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict';

import { beforeAll, expect, test } from '@jest/globals';
import { GraphQLObjectType, GraphQLSchema } from 'graphql';

import * as openAPIToGraphQL from '../src/index';

// Set up the schema first
const oas = require('./fixtures/petstore31.json');

let createdSchema: GraphQLSchema;
beforeAll(() => {
  return openAPIToGraphQL
    .createGraphQLSchema(oas, {
      softValidation: true,
    })
    .then(({ schema, report }) => {
      createdSchema = schema;
    })
    .catch(e => {
      console.log(e);
    });
});

test('Petstore 3.1 works', () => {
  const gqlTypes = Object.keys(
    (createdSchema.getTypeMap().Query as GraphQLObjectType).getFields()
  );
  expect(gqlTypes.length).toEqual(2);
});
