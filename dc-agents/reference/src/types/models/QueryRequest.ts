/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Query } from './Query';
import type { TableName } from './TableName';
import type { TableRelationships } from './TableRelationships';

export type QueryRequest = {
  query: Query;
  table: TableName;
  /**
   * The relationships between tables involved in the entire query request
   */
  table_relationships: Array<TableRelationships>;
};

