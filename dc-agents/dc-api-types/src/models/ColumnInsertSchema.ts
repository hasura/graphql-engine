/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ScalarType } from './ScalarType';

export type ColumnInsertSchema = {
  /**
   * The name of the column that this field should be inserted into
   */
  column: string;
  column_type: ScalarType;
  type: 'column';
};

