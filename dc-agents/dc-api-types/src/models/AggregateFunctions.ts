/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ScalarType } from './ScalarType';

/**
 * A map from aggregate function names to their result types.
 * Function and result type names must be valid GraphQL names.
 * Result type names must be defined scalar types declared in ScalarTypesCapabilities.
 *
 */
export type AggregateFunctions = Record<string, ScalarType>;
