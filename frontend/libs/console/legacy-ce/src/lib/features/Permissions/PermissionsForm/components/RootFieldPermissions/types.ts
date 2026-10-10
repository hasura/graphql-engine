import {
  QueryRootPermissionType,
  SubscriptionRootPermissionType,
} from '@hasura/shared/types';

export type QueryRootPermissionTypes = QueryRootPermissionType[] | null;
export type SubscriptionRootPermissionTypes =
  SubscriptionRootPermissionType[] | null;

export type PermissionRootType =
  QueryRootPermissionType | SubscriptionRootPermissionType;
export type PermissionRootTypes =
  Array<QueryRootPermissionType> | Array<SubscriptionRootPermissionType>;
