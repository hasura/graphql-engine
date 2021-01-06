import { RemoteSchemaPermissionsState } from './types';

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
