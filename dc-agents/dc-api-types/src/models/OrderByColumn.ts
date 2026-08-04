/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RedactionExpressionName } from './RedactionExpressionName';

export type OrderByColumn = {
  column: (Array<string> | string);
  redaction_expression?: RedactionExpressionName;
  type: 'column';
};

