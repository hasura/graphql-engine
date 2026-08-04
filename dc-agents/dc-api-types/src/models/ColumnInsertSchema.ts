/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ColumnType } from './ColumnType';
import type { ColumnValueGenerationStrategy } from './ColumnValueGenerationStrategy';

export type ColumnInsertSchema = {
  /**
   * The name of the column that this field should be inserted into
   */
  column: string;
  column_type: ColumnType;
  /**
   * Is the column nullable
   */
  nullable: boolean;
  type: 'column';
  value_generated?: ColumnValueGenerationStrategy;
};

