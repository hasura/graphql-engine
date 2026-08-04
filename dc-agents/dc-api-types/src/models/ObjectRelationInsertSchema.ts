/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

import type { ObjectRelationInsertionOrder } from './ObjectRelationInsertionOrder';

export type ObjectRelationInsertSchema = {
  insertion_order: ObjectRelationInsertionOrder;
  /**
   * The name of the object relationship over which the related row must be inserted
   */
  relationship: string;
  type: 'object_relation';
};

