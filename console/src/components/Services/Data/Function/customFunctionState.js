const asyncState = {
  isRequesting: false,
  isUntracking: false,
  isDeleting: false,
  isError: false,
  isFetching: false,
  isUpdating: false,
  isFetchError: null,
};

const functionData = {
  functionName: '',
  functionSchema: '',
  functionDefinition: '',
  configuration: {},
  setOffTable: '',
  setOffTableSchema: '',
  inputArgNames: [],
  inputArgTypes: [],
  ...asyncState,
};

export { functionData };
