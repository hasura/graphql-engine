/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { OrderByElement } from './OrderByElement';
import type { OrderByRelation } from './OrderByRelation';

export type OrderBy = {
  /**
   * The elements to order by, in priority order
   */
  elements: Array<OrderByElement>;
  /**
   * A map of relationships from the current query table to target tables. The key of the map is the relationship name. The relationships are used within the order by elements.
   */
  relations: Record<string, OrderByRelation>;
};

