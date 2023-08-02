/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ScalarType } from './ScalarType';
import type { SingleColumnAggregateFunction } from './SingleColumnAggregateFunction';

export type OrderBySingleColumnAggregate = {
  /**
   * The column to apply the aggregation function to
   */
  column: string;
  function: SingleColumnAggregateFunction;
  result_type: ScalarType;
  type: 'single_column_aggregate';
};

