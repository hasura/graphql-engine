/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { OrderType } from './OrderType';

export type OrderBy = {
  /**
   * Column to order by
   */
  column: string;
  ordering: OrderType;
};

