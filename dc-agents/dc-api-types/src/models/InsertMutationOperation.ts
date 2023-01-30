/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Expression } from './Expression';
import type { Field } from './Field';
import type { RowObject } from './RowObject';
import type { TableName } from './TableName';

export type InsertMutationOperation = {
  post_insert_check?: Expression;
  /**
   * The fields to return for the rows affected by this insert operation
   */
  returning_fields?: Record<string, Field> | null;
  /**
   * The rows to insert into the table
   */
  rows: Array<RowObject>;
  table: TableName;
  type: 'insert';
};

