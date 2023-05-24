/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnType } from './ColumnType';
import type { ColumnValueGenerationStrategy } from './ColumnValueGenerationStrategy';

export type ColumnInfo = {
  /**
   * Column description
   */
  description?: string | null;
  /**
   * Whether or not the column can be inserted into
   */
  insertable?: boolean;
  /**
   * Column name
   */
  name: string;
  /**
   * Is column nullable
   */
  nullable: boolean;
  type: ColumnType;
  /**
   * Whether or not the column can be updated
   */
  updatable?: boolean;
  value_generated?: ColumnValueGenerationStrategy;
};

