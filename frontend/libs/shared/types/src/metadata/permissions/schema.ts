import * as z from 'zod';

export const queryRootPermissionFields = [
  'select',
  'select_by_pk',
  'select_aggregate',
] as const;

export const subscriptionRootPermissionFields = [
  'select',
  'select_by_pk',
  'select_aggregate',
  'select_stream',
] as const;

export const permissionQueryRootFieldSchema = z.enum(queryRootPermissionFields);
export const permissionSubscriptionRootFieldSchema = z.enum(
  subscriptionRootPermissionFields,
);

export type QueryRootPermissionType = z.infer<
  typeof permissionQueryRootFieldSchema
>;
export type SubscriptionRootPermissionType = z.infer<
  typeof permissionSubscriptionRootFieldSchema
>;

export const dataQueryTypeSchema = z.union([
  z.literal(''),
  z.literal('insert'),
  z.literal('select'),
  z.literal('update'),
  z.literal('delete'),
]);
