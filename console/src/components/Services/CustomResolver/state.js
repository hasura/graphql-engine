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
  envName: '',
  headers: [],
  name: '',
  ...asyncState,
  editState: {
    id: -1,
    isModify: false,
    originalName: '',
  },
};

export { listState, addState };
