const asyncState = {
  isRequesting: false,
  isError: false,
  isFetching: false,
  isFetchError: null,
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

const permissionState = {
  rolePermissions: [
    {
      role: '',
      allowedTypes: {},
    },
  ],
  ...asyncState,
};

export { listState, addState, permissionState };
