const asyncState = {
  isRequesting: false,
  isError: false,
  isFetching: false,
  isFetchError: null,
  loadingRequests: {},
};

const listState = {
  remoteSchemas: [],
  filtered: [],
  searchQuery: '',
  viewRemoteSchema: '',
  ...asyncState,
};

const addState = {
  manualUrl: '',
  envName: null,
  headers: [],
  name: '',
  forwardClientHeaders: false,
  ...asyncState,
  editState: {
    id: -1,
    isModify: false,
    originalName: '',
    originalHeaders: [],
    originalUrl: '',
    originalEnvUrl: '',
    originalForwardClientHeaders: false,
  },
};

const defaultEditState = {
  role: '',
  allowedTypes: {},
  allowAll: false,
};

const permissionState = {
  existingPermissions: [{
    remote_schema_name: 'mahSchema',
    definition: {
      query: ['a', 'b']
    }
  }],
  editState: {
    ...defaultEditState,
    isEditting: false
  },
  currentRemoteSchemaName: '',
  ...asyncState,
};

export { listState, addState, permissionState };
