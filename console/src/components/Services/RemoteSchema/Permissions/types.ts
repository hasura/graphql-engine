import { GraphQLField } from 'graphql';
import { Dispatch } from '../../../../types';
import { Nullable } from '../../../Common/utils/tsUtils';

export const PERMISSIONS_OPEN_EDIT =
  'RemoteSchemas/Permissions/PERMISSIONS_OPEN_EDIT';
export const PERMISSIONS_CLOSE_EDIT =
  'RemoteSchemas/Permissions/PERMISSIONS_CLOSE_EDIT';
export const SET_ROLE_NAME = 'RemoteSchemas/Permissions/SET_ROLE_NAME';
export const SET_DEFAULTS = 'RemoteSchemas/Permissions/SET_DEFAULTS';
export const SET_SCHEMA_DEFINITION =
  'RemoteSchemas/Permissions/SET_SCHEMA_DEFINITION';
export const PERM_SELECT_BULK = 'RemoteSchemas/Permissions/PERM_SELECT_BULK';
export const PERM_DESELECT_BULK =
  'RemoteSchemas/Permissions/PERM_DESELECT_BULK';
export const PERM_RESET_BULK_SELECT =
  'RemoteSchemas/Permissions/PERM_RESET_BULK_SELECT';
export const MAKE_REQUEST = 'RemoteSchemas/Permissions/MAKE_REQUEST';
export const REQUEST_SUCCESS = 'RemoteSchemas/Permissions/REQUEST_SUCCESS';
export const REQUEST_FAILURE = 'RemoteSchemas/Permissions/REQUEST_FAILURE';

/*
 * Common types for remote schema permissions
 */

export type PermissionEdit = {
  isNew: boolean;
  newRole: string;
  role: string;
  filter: string;
};

export type SchemaPermissionsState = {
  isEditing: false;
  isFetching: false;
  permissionEdit: PermissionEdit;
  schemaDefinition: string;
  bulkSelect: string[];
};

export type PermissionsProps = {
  allRoles: string[];
  currentRemoteSchema: {
    name: string;
    permissions: Record<string, string>;
  };
  readOnlyMode: boolean;
  dispatch: Dispatch;
  isEditing: boolean;
  isFetching: boolean;
  bulkSelect: string[];
  permissionEdit: { isNewRole: boolean; role: string; newRole: string };
  schemaDefinition: string;
  fetchRoleList: () => void;
  setDefaults: () => void;
  permCloseEdit: () => void;
  saveRemoteSchemaPermission: (data: any) => void;
  removeRemoteSchemaPermission: (data: any) => void;
  setSchemaDefinition: (data: any) => void;
  permRemoveMultipleRoles: () => void;
  permOpenEdit: (
    role: string,
    newRole: boolean,
    existingPerms: boolean
  ) => void;
  permSetBulkSelect: (checked: boolean, role: string) => void;
  permSetRoleName: (name: string) => void;
};

export type RolePermissions = {
  roleName: string;
  permTypes: Record<string, any>;
  bulkSection: Record<string, any>;
  isNewRole?: boolean;
};

// TODO generic types -> seperate this

export type CustomFieldType = {
  args: any[];
  name: string;
  checked?: boolean;
  return?: string;
  children?: FieldType[];
};

export type FieldType = CustomFieldType & GraphQLField<any, any>;

export type DatasourceObject = {
  name: string;
  children: FieldType[]; // TODO extend type
};

export type PermWrapperProps = {
  allRoles: string[];
  allRemoteSchemas: any[];
  params: { [key: string]: string };
  viewRemoteSchema: (data: string) => void;
};

export type BulkSelectProps = {
  bulkSelect: any[];
  permRemoveMultipleRoles: () => void;
};
