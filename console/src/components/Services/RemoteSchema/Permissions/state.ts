import { SchemaPermissionsState } from './types';

const state: SchemaPermissionsState = {
  isEditing: false,
  isFetching: false,
  permissionEdit: {
    isNew: false,
    newRole: '',
    role: '',
    filter: '',
  },
  schemaDefinition: '',
  bulkSelect: [],
};

export default state;
