/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnInfo } from './ColumnInfo';

export type ObjectTypeDefinition = {
  /**
   * The columns of the type
   */
  columns: Array<ColumnInfo>;
  /**
   * The description of the type
   */
  description?: string;
  /**
   * The name of the type
   */
  name: string;
};

