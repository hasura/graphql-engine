export type QueryRootPermissionType =
  | 'select'
  | 'select_by_pk'
  | 'select_aggregate';

export type SubscriptionRootPermissionType =
  | 'select'
  | 'select_by_pk'
  | 'select_aggregate'
  | 'select_stream';

export type QueryRootPermissionTypes = QueryRootPermissionType[];
export type SubscriptionRootPermissionTypes = SubscriptionRootPermissionType[];

export type PermissionRootType = SubscriptionRootPermissionType;
export type PermissionRootTypes = SubscriptionRootPermissionTypes;

export type RootFieldPermissionsType = {
  subscriptionRootPermissions: SubscriptionRootPermissionTypes;
  queryRootPermissions: QueryRootPermissionTypes;
  onSubmitUpdate: (
    rootQueryType: string,
    permissionArray: PermissionRootTypes | null
  ) => void;
  hasEnabledAggregations: boolean;
  hasSelectedPrimaryKeys: boolean;
  currentlyEnabledColumns: string[];
  disabled: boolean;
};
