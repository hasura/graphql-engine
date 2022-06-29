/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RelationshipType } from './RelationshipType';

export type Relationship = {
  /**
   * A mapping between columns on the source table to columns on the target table
   */
  column_mapping: Record<string, string>;
  relationship_type: RelationshipType;
  /**
   * The name of the target table in the relationship
   */
  target_table: string;
};

