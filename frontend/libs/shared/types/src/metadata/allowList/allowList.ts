import { CollectionName } from '../queryCollections/queryCollections';

export interface AllowList {
  /** Name of a query collection to be added to the allow-list */
  collection: CollectionName;
  scope?:
    | {
        global: false;
        roles: string[];
      }
    | { global: true };
}
