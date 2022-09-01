/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Relationship } from './Relationship';
import type { TableName } from './TableName';

export type TableRelationships = {
  /**
   * A map of relationships from the source table to target tables. The key of the map is the relationship name
   */
  relationships: Record<string, Relationship>;
  source_table: TableName;
};

