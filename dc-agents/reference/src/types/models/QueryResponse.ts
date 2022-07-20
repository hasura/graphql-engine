/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { FieldValue } from './FieldValue';
import type { ScalarValue } from './ScalarValue';

export type QueryResponse = {
  /**
   * The results of the aggregates returned by the query
   */
  aggregates?: Record<string, ScalarValue> | null;
  /**
   * The rows returned by the query, corresponding to the query's fields
   */
  rows?: Array<Record<string, FieldValue>> | null;
};

