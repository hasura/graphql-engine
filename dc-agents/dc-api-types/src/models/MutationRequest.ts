/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { MutationOperation } from './MutationOperation';
import type { TableInsertSchema } from './TableInsertSchema';
import type { TableRelationships } from './TableRelationships';

export type MutationRequest = {
  /**
   * The schema by which to interpret row data specified in any insert operations in this request
   */
  insert_schema: Array<TableInsertSchema>;
  /**
   * The mutation operations to perform
   */
  operations: Array<MutationOperation>;
  /**
   * The relationships between tables involved in the entire mutation request
   */
  table_relationships: Array<TableRelationships>;
};

