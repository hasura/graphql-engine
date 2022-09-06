/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnFieldValue } from './ColumnFieldValue';
import type { NullColumnFieldValue } from './NullColumnFieldValue';

export type QueryResponse = {
  /**
   * The results of the aggregates returned by the query
   */
  aggregates?: Record<string, any> | null;
  /**
   * The rows returned by the query, corresponding to the query's fields
   */
  rows?: Array<Record<string, (ColumnFieldValue | QueryResponse | NullColumnFieldValue)>> | null;
};

