/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Relationship } from './Relationship';

export type InterpolatedRelationships = {
  /**
   * A map of relationships from the interpolated table to targets. The key of the map is the relationship name
   */
  relationships: Record<string, Relationship>;
  /**
   * The source interpolated query involved in the relationship
   */
  source_interpolated_query: string;
  type: 'interpolated';
};

