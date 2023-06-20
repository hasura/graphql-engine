// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict';

import { graphql } from 'graphql';
import { afterAll, beforeAll, expect, test } from '@jest/globals';

import * as openAPIToGraphQL from '../src/index';
import { Options } from '../src/types/options';

const oas = require('./fixtures/docusign.json');

test('Generate schema without problems', () => {
  const options: Options<any, any, any> = {
    strict: false,
  };
  return openAPIToGraphQL
    .createGraphQLSchema(oas, options)
    .then(({ schema }) => {
      expect(schema).toBeTruthy();
    });
});
