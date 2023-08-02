/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ScalarType } from './ScalarType';
import type { UpdateColumnOperatorName } from './UpdateColumnOperatorName';

export type CustomUpdateColumnOperatorRowUpdate = {
  /**
   * The name of the column in the row
   */
  column: string;
  operator_name: UpdateColumnOperatorName;
  type: 'custom_operator';
  /**
   * The value to use with the column operator
   */
  value: any;
  value_type: ScalarType;
};

