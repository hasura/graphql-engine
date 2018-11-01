const asyncState = {
  isRequesting: false,
  isError: false,
};

const listState = {
  resolvers: [],
  ...asyncState,
};

const addState = {
  manualUrl: '',
  envName: '',
  headers: [],
  name: '',
  ...asyncState,
};

export { listState, addState };
