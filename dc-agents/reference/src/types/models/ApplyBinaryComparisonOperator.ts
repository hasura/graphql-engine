/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { BinaryComparisonOperator } from './BinaryComparisonOperator';
import type { ComparisonColumn } from './ComparisonColumn';
import type { ComparisonValue } from './ComparisonValue';

export type ApplyBinaryComparisonOperator = {
  column: ComparisonColumn;
  operator: BinaryComparisonOperator;
  type: 'binary_op';
  value: ComparisonValue;
};

