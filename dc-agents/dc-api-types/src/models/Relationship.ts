/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RelationshipType } from './RelationshipType';
import type { Target } from './Target';

export type Relationship = {
  /**
   * A mapping between columns on the source table to columns on the target table
   */
  column_mapping: Record<string, (Array<string> | string)>;
  relationship_type: RelationshipType;
  target: Target;
};

