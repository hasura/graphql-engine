/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Aggregate } from './Aggregate';
import type { Expression } from './Expression';
import type { Field } from './Field';
import type { OrderBy } from './OrderBy';

export type Query = {
  /**
   * Aggregate fields of the query
   */
  aggregates?: Record<string, Aggregate> | null;
  /**
   * Optionally limit the maximum number of rows considered while applying aggregations. This limit does not apply to returned rows.
   */
  aggregates_limit?: number | null;
  /**
   * Fields of the query
   */
  fields?: Record<string, Field> | null;
  /**
   * Optionally limit the maximum number of returned rows. This limit does not apply to records considered while apply aggregations.
   */
  limit?: number | null;
  /**
   * Optionally offset from the Nth result. This applies to both row and aggregation results.
   */
  offset?: number | null;
  order_by?: OrderBy;
  where?: Expression;
};

