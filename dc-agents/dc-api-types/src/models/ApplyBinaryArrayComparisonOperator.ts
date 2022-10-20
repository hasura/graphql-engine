/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BinaryArrayComparisonOperator } from './BinaryArrayComparisonOperator';
import type { ComparisonColumn } from './ComparisonColumn';
import type { ScalarType } from './ScalarType';

export type ApplyBinaryArrayComparisonOperator = {
  column: ComparisonColumn;
  operator: BinaryArrayComparisonOperator;
  type: 'binary_arr_op';
  value_type: ScalarType;
  values: Array<any>;
};

