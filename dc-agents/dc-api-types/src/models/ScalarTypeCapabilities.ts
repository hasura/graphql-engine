/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { AggregateFunctions } from './AggregateFunctions';
import type { ComparisonOperators } from './ComparisonOperators';

/**
 * Capabilities of a scalar type.
 * comparison_operators: The comparison operators supported by the scalar type.
 * aggregate_functions: The aggregate functions supported by the scalar type.
 *
 */
export type ScalarTypeCapabilities = {
  aggregate_functions?: AggregateFunctions;
  comparison_operators?: ComparisonOperators;
};

