/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Expression } from './Expression';

export type OrExpression = {
  expressions: Array<Expression>;
  type: 'or';
};

