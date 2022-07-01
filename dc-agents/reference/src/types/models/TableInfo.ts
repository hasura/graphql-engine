/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnInfo } from './ColumnInfo';

export type TableInfo = {
  /**
   * The columns of the table
   */
  columns: Array<ColumnInfo>;
  /**
   * Description of the table
   */
  description?: string | null;
  /**
   * The name of the table
   */
  name: string;
  /**
   * The primary key of the table
   */
  primary_key?: Array<string> | null;
};

