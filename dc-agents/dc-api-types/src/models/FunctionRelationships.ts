/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FunctionName } from './FunctionName';
import type { Relationship } from './Relationship';

export type FunctionRelationships = {
  /**
   * A map of relationships from the source table to target tables. The key of the map is the relationship name
   */
  relationships: Record<string, Relationship>;
  source_function: FunctionName;
  type: 'function';
};

