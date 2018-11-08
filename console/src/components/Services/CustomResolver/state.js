const asyncState = {
  isRequesting: false,
  isError: false,
};

const listState = {
  resolvers: [],
  filtered: [],
  searchQuery: '',
  viewResolver: '',
  ...asyncState,
};

const addState = {
  manualUrl: '',
  envName: null,
  headers: [],
  name: '',
  ...asyncState,
  editState: {
    id: -1,
    isModify: false,
    originalName: '',
    originalHeaders: [],
    originalUrl: '',
    originalEnvUrl: '',
  },
};

export { listState, addState };
