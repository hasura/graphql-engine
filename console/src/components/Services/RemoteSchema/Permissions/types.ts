import {
  GraphQLField,
  GraphQLArgument,
  GraphQLInputFieldMap,
  GraphQLEnumValue,
  GraphQLType,
} from 'graphql';
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

export type PermOpenEditType = (
  role: string,
  newRole: boolean,
  existingPerms: boolean
) => void;

export type PermissionEdit = {
  newRole: string;
  isNewRole: boolean;
  isNewPerm: boolean;
  role: string;
  filter?: Record<string, string>;
};

export type ArgTreeType = {
  [key: string]: string | number | ArgTreeType;
};
export type Actions = {
  setSchemaDefinition: (data: string) => void;
  permOpenEdit: PermOpenEditType;
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

export type RolePermissions = {
  roleName: string;
  permTypes: Record<string, any>;
  bulkSection: Record<string, any>;
  isNewRole?: boolean;
};

export type PermissionsType = {
  definition: { schema: string };
  role: string;
  remote_schema_name: string;
  comment: string | null;
};

export type ChildArgumentType = {
  children?: GraphQLInputFieldMap | GraphQLEnumValue[];
  path?: string;
  childrenType?: GraphQLType;
};

export type CustomFieldType = {
  name: string;
  checked: boolean;
  args?: Record<string, GraphQLArgument>;
  return?: string;
  typeName?: string;
  children?: FieldType[];
  defaultValue?: any;
  isInputObjectType?: boolean;
  parentName?: string;
};

export type FieldType = CustomFieldType & GraphQLField<any, any>;

export type RemoteSchemaFields =
  | {
      name: string;
      typeName: string;
      children: FieldType[] | CustomFieldType[];
    }
  | FieldType;

export type ExpandedItems = {
  [key: string]: boolean;
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
