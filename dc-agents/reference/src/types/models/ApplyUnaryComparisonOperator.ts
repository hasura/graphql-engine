/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ComparisonColumn } from './ComparisonColumn';
import type { UnaryComparisonOperator } from './UnaryComparisonOperator';

export type ApplyUnaryComparisonOperator = {
  column: ComparisonColumn;
  operator: UnaryComparisonOperator;
  type: 'unary_op';
};

