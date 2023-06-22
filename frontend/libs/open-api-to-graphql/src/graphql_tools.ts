// Copyright IBM Corp. 2018. All Rights Reserved.
// Node module: openapi-to-graphql
// This file is licensed under the MIT License.
// License text available at https://opensource.org/licenses/MIT

/**
 * Utilities related to GraphQL.
 */

import { GraphQLObjectType, GraphQLString } from 'graphql';

/**
 * Returns empty GraphQLObjectType.
 */
export function getEmptyObjectType(name: string): GraphQLObjectType {
  return new GraphQLObjectType({
    name: name + 'Placeholder',
    description: 'Placeholder object',
    fields: {
      message: {
        type: GraphQLString,
        description: 'Placeholder field',
        resolve: () => {
          return 'This is a placeholder field.';
        },
      },
    },
  });
}
