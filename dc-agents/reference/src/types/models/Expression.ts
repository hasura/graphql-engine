/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { AndExpression } from './AndExpression';
import type { ApplyBinaryArrayComparisonOperator } from './ApplyBinaryArrayComparisonOperator';
import type { ApplyBinaryComparisonOperator } from './ApplyBinaryComparisonOperator';
import type { ApplyUnaryComparisonOperator } from './ApplyUnaryComparisonOperator';
import type { NotExpression } from './NotExpression';
import type { OrExpression } from './OrExpression';

export type Expression = (ApplyBinaryArrayComparisonOperator | OrExpression | ApplyUnaryComparisonOperator | ApplyBinaryComparisonOperator | NotExpression | AndExpression);

