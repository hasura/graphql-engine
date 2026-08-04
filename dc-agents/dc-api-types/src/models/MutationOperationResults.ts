/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnFieldValue } from './ColumnFieldValue';
import type { NullColumnFieldValue } from './NullColumnFieldValue';
import type { QueryResponse } from './QueryResponse';

export type MutationOperationResults = {
  /**
   * The number of rows affected by the mutation operation
   */
  affected_rows: number;
  /**
   * The rows affected by the mutation operation
   */
  returning?: Array<Record<string, (ColumnFieldValue | QueryResponse | NullColumnFieldValue)>> | null;
};

