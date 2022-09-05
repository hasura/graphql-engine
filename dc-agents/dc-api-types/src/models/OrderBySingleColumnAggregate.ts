/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { SingleColumnAggregateFunction } from './SingleColumnAggregateFunction';

export type OrderBySingleColumnAggregate = {
  /**
   * The column to apply the aggregation function to
   */
  column: string;
  function: SingleColumnAggregateFunction;
  type: 'single_column_aggregate';
};

