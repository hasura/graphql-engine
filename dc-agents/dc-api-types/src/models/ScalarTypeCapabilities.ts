/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { AggregateFunctions } from './AggregateFunctions';
import type { GraphQLName } from './GraphQLName';

/**
 * Capabilities of a scalar type.
 * comparison_type: Name of the GraphQL input object to be used for comparison operations on the scalar type. The input object type must be defined in the `graphql_schema`.
 * aggregate_functions: The aggregate functions supported by the scalar type.
 *
 */
export type ScalarTypeCapabilities = {
  aggregate_functions?: AggregateFunctions;
  comparison_type?: GraphQLName;
};

