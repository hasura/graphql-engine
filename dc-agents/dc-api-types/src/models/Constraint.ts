/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { TableName } from './TableName';

export type Constraint = {
  /**
   * The columns on which you want want to define the foreign key.
   */
  column_mapping: Record<string, (Array<string> | string)>;
  foreign_table: TableName;
};

