import { string } from 'prop-types';
import { SchemaPermissionsState } from './types';

const defaultSchemaDefSdl = `type SampleType {
  user_id: Int
  name: String
}`;

const state: SchemaPermissionsState = {
  isEditing: false,
  isFetching: false,
  permissionEdit: {
    isNew: false,
    newRole: '',
    role: '',
    filter: '',
  },
  // schemaDefinition: {
  //   value: defaultSchemaDefSdl,
  //   error: null,
  //   timer: null,
  //   ast: null,
  // },
  schemaDefinition: '',
  bulkSelect: [],
};

export { defaultSchemaDefSdl };
export default state;
