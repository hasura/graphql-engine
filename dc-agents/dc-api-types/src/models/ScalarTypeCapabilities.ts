/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { AggregateFunctions } from './AggregateFunctions';
import type { ComparisonOperators } from './ComparisonOperators';
import type { GraphQLType } from './GraphQLType';
import type { UpdateColumnOperators } from './UpdateColumnOperators';

/**
 * Capabilities of a scalar type.
 * comparison_operators: The comparison operators supported by the scalar type.
 * aggregate_functions: The aggregate functions supported by the scalar type.
 * update_column_operators: The update column operators supported by the scalar type.
 * graphql_type: Associates the custom scalar type with one of the built-in GraphQL scalar types.  If a `graphql_type` is specified then HGE will use the parser for that built-in type when parsing values of the custom type. If not given then any JSON value will be accepted.
 *
 */
export type ScalarTypeCapabilities = {
  aggregate_functions?: AggregateFunctions;
  comparison_operators?: ComparisonOperators;
  graphql_type?: GraphQLType;
  update_column_operators?: UpdateColumnOperators;
};

