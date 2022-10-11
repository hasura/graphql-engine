/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Expression } from './Expression';

export type AndExpression = {
  expressions: Array<Expression>;
  type: 'and';
};

