const defaultSchemaDefSdl = `type SampleType {
  user_id: Int
  name: String
}`;

const state = {
  isEditing: false,
  isFetching: false,
  permissionEdit: {
    isNew: false,
    newRole: '',
    role: '',
    filter: '',
  },
  schemaDefinition: {
    sdl: defaultSchemaDefSdl,
    error: '',
    timer: null,
    ast: null,
  },
  bulkSelect: [],
};

export { defaultSchemaDefSdl };
export default state;
