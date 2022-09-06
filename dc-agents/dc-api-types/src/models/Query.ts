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
   * Fields of the query
   */
  fields?: Record<string, Field> | null;
  /**
   * Optionally limit to N results
   */
  limit?: number | null;
  /**
   * Optionally offset from the Nth result
   */
  offset?: number | null;
  order_by?: OrderBy;
  where?: Expression;
};

