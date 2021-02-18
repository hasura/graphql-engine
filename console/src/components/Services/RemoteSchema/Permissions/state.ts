import { PermissionEdit } from './types';

export type RemoteSchemaPermissionsState = {
  isEditing: false;
  isFetching: false;
  permissionEdit: PermissionEdit;
  schemaDefinition: string;
  bulkSelect: string[];
};

const state: RemoteSchemaPermissionsState = {
  isEditing: false,
  isFetching: false,
  permissionEdit: {
    newRole: '',
    isNewRole: false,
    isNewPerm: false,
    role: '',
  },
  schemaDefinition: '',
  bulkSelect: [],
};

export default state;
