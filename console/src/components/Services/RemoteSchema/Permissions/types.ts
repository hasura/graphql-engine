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

export type SchemaDefinition = {
  value: Nullable<string>;
  error?: Nullable<Error>;
  timer?: Nullable<NodeJS.Timeout>;
  ast?: Nullable<Record<string, any>>;
};

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
  // schemaDefinition: SchemaDefinition;
  schemaDefinition : string;
  bulkSelect: string[];
};

export type PermissionsProps = {
  currentRemoteSchema: {
    name: string;
    permissions: Record<string, string>;
  };
  readOnlyMode: boolean;
  dispatch: Dispatch;
  isEditing: boolean;
  isFetching: boolean;
  allRoles: string[];
  bulkSelect: string[];
  permissionEdit: { isNewRole: boolean; role: string; newRole: string };
  schemaDefinition: any; // TODO: provide the right type here
};

export type RolePermissions = {
  roleName: string;
  permTypes: Record<string, any>;
  bulkSection: Record<string, any>;
  isNewRole?: boolean;
};
