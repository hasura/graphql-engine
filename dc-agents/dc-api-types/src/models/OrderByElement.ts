/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { OrderByTarget } from './OrderByTarget';
import type { OrderDirection } from './OrderDirection';

export type OrderByElement = {
  order_direction: OrderDirection;
  target: OrderByTarget;
  /**
   * The relationship path from the current query table to the table that contains the target to order by. This is always non-empty for aggregate order by targets
   */
  target_path: Array<string>;
};

