import { SchemaPermissionsState } from './types';

const state: SchemaPermissionsState = {
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
