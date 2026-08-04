/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { RedactionExpressionName } from './RedactionExpressionName';
import type { ScalarType } from './ScalarType';
import type { SingleColumnAggregateFunction } from './SingleColumnAggregateFunction';

export type SingleColumnAggregate = {
  /**
   * The column to apply the aggregation function to
   */
  column: string;
  function: SingleColumnAggregateFunction;
  redaction_expression?: RedactionExpressionName;
  result_type: ScalarType;
  type: 'single_column';
};

