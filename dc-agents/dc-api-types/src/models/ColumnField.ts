/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RedactionExpressionName } from './RedactionExpressionName';
import type { ScalarType } from './ScalarType';

export type ColumnField = {
  column: string;
  column_type: ScalarType;
  redaction_expression?: RedactionExpressionName;
  type: 'column';
};

