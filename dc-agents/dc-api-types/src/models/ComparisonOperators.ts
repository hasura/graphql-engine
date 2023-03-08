/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ScalarType } from './ScalarType';

/**
 * A map from comparison operator names to their argument types.
 * Operator and argument type names must be valid GraphQL names.
 * Argument type names must be defined scalar types declared in ScalarTypesCapabilities.
 *
 */
export type ComparisonOperators = Record<string, ScalarType>;
