/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { UpdateColumnOperatorDefinition } from './UpdateColumnOperatorDefinition';

/**
 * A map from update column operator names to their definitions.
 * Operator names must be valid GraphQL names.
 *
 */
export type UpdateColumnOperators = Record<string, UpdateColumnOperatorDefinition>;
