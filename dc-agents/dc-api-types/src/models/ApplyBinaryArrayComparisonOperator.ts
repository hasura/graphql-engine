/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BinaryArrayComparisonOperator } from './BinaryArrayComparisonOperator';
import type { ComparisonColumn } from './ComparisonColumn';

export type ApplyBinaryArrayComparisonOperator = {
  column: ComparisonColumn;
  operator: BinaryArrayComparisonOperator;
  type: 'binary_arr_op';
  values: Array<any>;
};

