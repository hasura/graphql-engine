/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RedactionExpressionName } from './RedactionExpressionName';

export type OrderByColumn = {
  column: (string | Array<string>);
  redaction_expression?: RedactionExpressionName;
  type: 'column';
};

