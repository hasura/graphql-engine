/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Expression } from './Expression';
import type { Field } from './Field';
import type { RowUpdate } from './RowUpdate';
import type { TableName } from './TableName';

export type UpdateMutationOperation = {
  post_update_check?: Expression;
  /**
   * The fields to return for the rows affected by this update operation
   */
  returning_fields?: Record<string, Field> | null;
  table: TableName;
  type: 'update';
  /**
   * The updates to make to the matched rows in the table
   */
  updates: Array<RowUpdate>;
  where?: Expression;
};

