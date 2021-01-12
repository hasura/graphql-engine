import { GraphQLField, GraphQLSchema, GraphQLArgument } from 'graphql';
import { Action as ReduxAction } from 'redux';
import { Dispatch } from '../../../../types';

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

type permOpenEdit = (
  role: string,
  newRole: boolean,
  existingPerms: boolean
) => void;

export type PermissionEdit = {
  newRole: string;
  isNewRole: boolean;
  isNewPerm: boolean;
  role: string;
};

export type RemoteSchemaPermissionsState = {
  isEditing: false;
  isFetching: false;
  permissionEdit: PermissionEdit;
  schemaDefinition: string;
  bulkSelect: string[];
};

export type argTreeType = {
  [key: string]: string | number | argTreeType;
};
export type Actions = {
  setSchemaDefinition: (data: string) => void;
  permOpenEdit: permOpenEdit;
  permCloseEdit: () => void;
  permSetBulkSelect: (checked: boolean, role: string) => void;
  permSetRoleName: (name: string) => void;
  dispatch: Dispatch;
  fetchRoleList: () => void;
  setDefaults: () => void;
  saveRemoteSchemaPermission: (data: any) => void;
  removeRemoteSchemaPermission: (data: any) => void;
  permRemoveMultipleRoles: () => void;
};

export type PermissionsProps = {
  allRoles: string[];
  currentRemoteSchema: {
    name: string;
    permissions: Permissions[];
  };
  bulkSelect: string[];
  readOnlyMode: boolean;
  permissionEdit: PermissionEdit;
  isEditing: boolean;
  isFetching: boolean;
  schemaDefinition: string;
  setSchemaDefinition: (data: string) => void;
  permOpenEdit: permOpenEdit;
  permCloseEdit: () => void;
  permSetBulkSelect: (checked: boolean, role: string) => void;
  permSetRoleName: (name: string) => void;
  dispatch: Dispatch;
  fetchRoleList: () => void;
  setDefaults: () => void;
  saveRemoteSchemaPermission: (data: any) => void;
  removeRemoteSchemaPermission: (data: any) => void;
  permRemoveMultipleRoles: () => void;
};

export type PermissionsTableProps = {
  setSchemaDefinition: (data: string) => void;
  permOpenEdit: permOpenEdit;
  permCloseEdit: () => void;
  permSetBulkSelect: (checked: boolean, role: string) => void;
  permSetRoleName: (name: string) => void;
  allRoles: string[];
  currentRemoteSchema: {
    name: string;
    permissions: Permissions[];
  };
  bulkSelect: string[];
  readOnlyMode: boolean;
  permissionEdit: PermissionEdit;
  isEditing: boolean;
};

export type PermissionEditorProps = {
  permissionEdit: PermissionEdit;
  isEditing: boolean;
  isFetching: boolean;
  schemaDefinition: string;
  datasource: any;
  schema: GraphQLSchema | null;
  setSchemaDefinition: (data: string) => void;
  permCloseEdit: () => void;
  saveRemoteSchemaPermission: (data: any) => void;
  removeRemoteSchemaPermission: (data: any) => void;
};

export type RolePermissions = {
  roleName: string;
  permTypes: Record<string, any>;
  bulkSection: Record<string, any>;
  isNewRole?: boolean;
};

export type Permissions = {
  definition: { schema: string };
  role_name: string;
  remote_schema_name: string;
  comment: string | null;
};

// TODO generic types -> seperate this

export type CustomFieldType = {
  name: string;
  args?: GraphQLArgument[];
  checked?: boolean;
  return?: string;
  typeName?: string;
  children?: FieldType[];
};

export type FieldType = CustomFieldType & GraphQLField<any, any>;

export type DatasourceObject = {
  name: string;
  typeName: string;
  children: FieldType[]; // TODO extend type
};

export type PermWrapperProps = {
  allRoles: string[];
  allRemoteSchemas: { [key: string]: any }[];
  params: { [key: string]: string };
  viewRemoteSchema: (data: string) => void;
};

export type BulkSelectProps = {
  bulkSelect: string[];
  permRemoveMultipleRoles: () => void;
};

export type RSPTreeComponentProps = {
  list: FieldType[];
  depth?: number;
  setState: (d: FieldType[], t?: FieldType) => void;
  onExpand?: () => void;
};

export type ExpandedItems = {
  [key: string]: boolean;
};

export interface FieldProps {
  i: FieldType;
  setItem: (e: FieldType) => void;
  onExpand?: () => void;
}

export type RSPContainerProps = {
  params: { [key: string]: string };
  allRemoteSchemas: { [key: string]: any }[];
  tabName: string;
  viewRemoteSchema: (data: any) => void;
};

/*
 * Redux Action types
 */

export interface PermOpenEdit extends ReduxAction {
  type: typeof PERMISSIONS_OPEN_EDIT;
  role: string;
  isNewRole: boolean;
  isNewPerm: boolean;
}
export interface PermCloseEdit extends ReduxAction {
  type: typeof PERMISSIONS_CLOSE_EDIT;
}
export interface PermSetRoleName extends ReduxAction {
  type: typeof SET_ROLE_NAME;
  rolename: string;
}
export interface SetDefaults extends ReduxAction {
  type: typeof SET_DEFAULTS;
}
export interface SetSchemaDefinition extends ReduxAction {
  type: typeof SET_SCHEMA_DEFINITION;
  definition: string;
}
export interface PermSelectBulk extends ReduxAction {
  type: typeof PERM_SELECT_BULK;
  selectedRole: string;
}
export interface PermDeslectBulk extends ReduxAction {
  type: typeof PERM_DESELECT_BULK;
  selectedRole: string;
}
export interface PermResetBulkSelect extends ReduxAction {
  type: typeof PERM_RESET_BULK_SELECT;
}
export interface MakeRequest extends ReduxAction {
  type: typeof MAKE_REQUEST;
}
export interface SetRequestSuccess extends ReduxAction {
  type: typeof REQUEST_SUCCESS;
}
export interface SetRequestFailure extends ReduxAction {
  type: typeof REQUEST_FAILURE;
}

export type RSPEvents =
  | PermOpenEdit
  | PermCloseEdit
  | PermSetRoleName
  | SetDefaults
  | SetSchemaDefinition
  | PermSelectBulk
  | PermDeslectBulk
  | PermResetBulkSelect
  | MakeRequest
  | SetRequestFailure
  | SetRequestSuccess;
