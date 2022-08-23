/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnInfo } from './ColumnInfo';
import type { TableName } from './TableName';
import type { Constraint } from './Constraint';

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

  /**
   * A record of Constraint names to Constraints for foreign key
   * relationships.
   */
  foreign_keys?: Record<string, Constraint> | null;
};

