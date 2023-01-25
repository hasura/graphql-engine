import { RootKeyValues } from './RootFieldPermissions';

export type QueryRootPermissionType =
  | 'select'
  | 'select_by_pk'
  | 'select_aggregate';

export type SubscriptionRootPermissionType =
  | 'select'
  | 'select_by_pk'
  | 'select_aggregate'
  | 'select_stream';

export type QueryRootPermissionTypes = QueryRootPermissionType[] | null;
export type SubscriptionRootPermissionTypes =
  | SubscriptionRootPermissionType[]
  | null;

export type PermissionRootType =
  | QueryRootPermissionType
  | SubscriptionRootPermissionType;
export type PermissionRootTypes =
  | QueryRootPermissionTypes
  | SubscriptionRootPermissionTypes;
export type CombinedPermissionRootTypes = QueryRootPermissionTypes &
  SubscriptionRootPermissionTypes;

export type RootFieldPermissionsType = {
  hasEnabledAggregations: boolean;
  hasSelectedPrimaryKeys: boolean;
  queryRootFields: QueryRootPermissionTypes;
  subscriptionRootFields: SubscriptionRootPermissionTypes;
  updateFormValues: (key: RootKeyValues, value: PermissionRootTypes) => void;
};
