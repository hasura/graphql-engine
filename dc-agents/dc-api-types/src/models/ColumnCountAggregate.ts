/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RedactionExpressionName } from './RedactionExpressionName';

export type ColumnCountAggregate = {
  /**
   * The column to apply the count aggregate function to
   */
  column: string;
  /**
   * Whether or not only distinct items should be counted
   */
  distinct: boolean;
  redaction_expression?: RedactionExpressionName;
  type: 'column_count';
};

