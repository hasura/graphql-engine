/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { Query } from './Query';

export type RelationshipField = {
  query: Query;
  /**
   * The name of the relationship to follow for the subquery
   */
  relationship: string;
  type: 'relationship';
};

