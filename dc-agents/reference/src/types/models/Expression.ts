/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { AndExpression } from './AndExpression';
import type { ApplyBinaryArrayComparisonExpression } from './ApplyBinaryArrayComparisonExpression';
import type { ApplyBinaryComparisonOperator } from './ApplyBinaryComparisonOperator';
import type { ApplyUnaryComparisonOperator } from './ApplyUnaryComparisonOperator';
import type { NotExpression } from './NotExpression';
import type { OrExpression } from './OrExpression';

export type Expression = (AndExpression | OrExpression | NotExpression | ApplyBinaryComparisonOperator | ApplyBinaryArrayComparisonExpression | ApplyUnaryComparisonOperator);

