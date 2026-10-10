// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

'use strict';

import { expect, test } from 'vitest';

import * as openAPIToGraphQL from '../src/index';

const oas = require('./fixtures/docusign.json');

test('Generate schema without problems', { retry: 1, timeout: 10000 }, () => {
  const options = {
    strict: false,
  };
  return openAPIToGraphQL
    .createGraphQLSchema(oas, options)
    .then(({ schema }) => {
      expect(schema).toBeTruthy();
    });
});
