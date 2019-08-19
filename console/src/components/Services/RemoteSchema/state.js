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

const defaultRolePermission = {
  role: '',
  allowedTypes: {},
  allowAll: false,
};

const permissionState = {
  rolePermissions: [JSON.parse(JSON.stringify(defaultRolePermission))],
  currentRemoteSchemaName: '',
  ...asyncState,
};

export { listState, addState, permissionState };
