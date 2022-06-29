/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Relationship } from './Relationship';

export type TableRelationships = {
  /**
   * A map of relationships from the source table to target tables. The key of the map is the relationship name
   */
  relationships: Record<string, Relationship>;
  /**
   * The name of the source table in the relationship
   */
  source_table: string;
};

