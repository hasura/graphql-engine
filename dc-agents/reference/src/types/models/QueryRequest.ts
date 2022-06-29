/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Query } from './Query';
import type { TableRelationships } from './TableRelationships';

export type QueryRequest = {
  query: Query;
  /**
   * The name of the table to query
   */
  table: string;
  /**
   * The relationships between tables involved in the entire query request
   */
  table_relationships: Array<TableRelationships>;
};

