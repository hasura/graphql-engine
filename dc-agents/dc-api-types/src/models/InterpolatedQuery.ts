/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { InterpolatedItem } from './InterpolatedItem';

export type InterpolatedQuery = {
  /**
   * An id associated with the interpolated query - Should be unique across the request
   */
  id: string;
  /**
   * Interpolated items in the query
   */
  items: Array<InterpolatedItem>;
};

