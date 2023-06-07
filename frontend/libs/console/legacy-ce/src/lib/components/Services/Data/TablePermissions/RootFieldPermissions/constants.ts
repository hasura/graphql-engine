import {
  QueryRootPermissionType,
  SubscriptionRootPermissionType,
} from './types';

export const queryRootPermissionFields: QueryRootPermissionType[] = [
  'select',
  'select_by_pk',
  'select_aggregate',
];

export const subscriptionRootPermissionFields: SubscriptionRootPermissionType[] =
  ['select', 'select_by_pk', 'select_aggregate', 'select_stream'];
