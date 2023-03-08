import { Permission } from '../../hasura-metadata-types';

export const permissionToKey = {
  insert: 'insert_permissions',
  select: 'select_permissions',
  update: 'update_permissions',
  delete: 'delete_permissions',
} as const;

export const metadataPermissionKeys = [
  'insert_permissions',
  'select_permissions',
  'update_permissions',
  'delete_permissions',
] as const;

export const keyToPermission = {
  insert_permissions: 'insert',
  select_permissions: 'select',
  update_permissions: 'update',
  delete_permissions: 'delete',
} as const;

export const isPermission = (props: {
  key: string;
  value: any;
}): props is {
  key: (typeof metadataPermissionKeys)[number];
  value: Permission[];
} => props.key in keyToPermission;
