/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnInfo } from './ColumnInfo';
import type { TableName } from './TableName';

export type TableInfo = {
  /**
   * The columns of the table
   */
  columns: Array<ColumnInfo>;
  /**
   * Description of the table
   */
  description?: string | null;
  name: TableName;
  /**
   * The primary key of the table
   */
  primary_key?: Array<string> | null;
};

