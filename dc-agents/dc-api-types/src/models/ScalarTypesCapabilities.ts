/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ScalarTypeCapabilities } from './ScalarTypeCapabilities';

/**
 * A map from scalar type names to their capabilities.
 * Keys must be valid GraphQL names and must be defined as scalar types in the `graphql_schema`
 *
 */
export type ScalarTypesCapabilities = Record<string, ScalarTypeCapabilities>;
