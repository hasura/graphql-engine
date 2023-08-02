/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Expression } from './Expression';
import type { Field } from './Field';
import type { OrderBy } from './OrderBy';

export type NestedArrayField = {
  field: Field;
  /**
   * Optionally limit the maximum number of returned elements of the array
   */
  limit?: number | null;
  /**
   * Optionally skip the first n elements of the array
   */
  offset?: number | null;
  order_by?: OrderBy;
  type: 'array';
  where?: Expression;
};

