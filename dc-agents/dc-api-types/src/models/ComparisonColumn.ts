/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RedactionExpressionName } from './RedactionExpressionName';
import type { ScalarType } from './ScalarType';

export type ComparisonColumn = {
  column_type: ScalarType;
  /**
   * The name of the column
   */
  name: (Array<string> | string);
  /**
   * The path to the table that contains the specified column. Missing or empty array means the current table. ["$"] means the query table. No other values are supported at this time.
   */
  path?: Array<string>;
  redaction_expression?: RedactionExpressionName;
};

