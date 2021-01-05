import { RemoteSchemaPermissionsState } from './types';

const state: RemoteSchemaPermissionsState = {
  isEditing: false,
  isFetching: false,
  permissionEdit: {
    newRole: '',
    isNewRole: false,
    isNewPerm: false,
    role: '',
    filter: '',
  },
  schemaDefinition: '',
  bulkSelect: [],
};

export default state;
