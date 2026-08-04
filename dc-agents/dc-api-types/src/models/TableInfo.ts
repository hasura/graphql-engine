/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnInfo } from './ColumnInfo';
import type { Constraint } from './Constraint';
import type { TableName } from './TableName';
import type { TableType } from './TableType';

export type TableInfo = {
  /**
   * The columns of the table
   */
  columns?: Array<ColumnInfo>;
  /**
   * Whether or not existing rows can be deleted in the table
   */
  deletable?: boolean;
  /**
   * Description of the table
   */
  description?: string | null;
  /**
   * Foreign key constraints
   */
  foreign_keys?: Record<string, Constraint>;
  /**
   * Whether or not new rows can be inserted into the table
   */
  insertable?: boolean;
  name: TableName;
  /**
   * The primary key of the table
   */
  primary_key?: Array<string>;
  type?: TableType;
  /**
   * Whether or not existing rows can be updated in the table
   */
  updatable?: boolean;
};

