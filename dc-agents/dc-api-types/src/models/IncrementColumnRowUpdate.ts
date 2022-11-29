/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ScalarType } from './ScalarType';

export type IncrementColumnRowUpdate = {
  /**
   * The name of the column in the row
   */
  column: string;
  type: 'increment';
  /**
   * The value to use for the column
   */
  value: any;
  value_type: ScalarType;
};

