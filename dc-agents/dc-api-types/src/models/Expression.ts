/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { AndExpression } from './AndExpression';
import type { ApplyBinaryArrayComparisonOperator } from './ApplyBinaryArrayComparisonOperator';
import type { ApplyBinaryComparisonOperator } from './ApplyBinaryComparisonOperator';
import type { ApplyUnaryComparisonOperator } from './ApplyUnaryComparisonOperator';
import type { ExistsExpression } from './ExistsExpression';
import type { NotExpression } from './NotExpression';
import type { OrExpression } from './OrExpression';

export type Expression = (ApplyBinaryComparisonOperator | NotExpression | ExistsExpression | OrExpression | ApplyBinaryArrayComparisonOperator | ApplyUnaryComparisonOperator | AndExpression);

