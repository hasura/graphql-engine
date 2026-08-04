/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Expression } from './Expression';
import type { Field } from './Field';
import type { TableName } from './TableName';

export type DeleteMutationOperation = {
  /**
   * The fields to return for the rows affected by this delete operation
   */
  returning_fields?: Record<string, Field> | null;
  table: TableName;
  type: 'delete';
  where?: Expression;
};

