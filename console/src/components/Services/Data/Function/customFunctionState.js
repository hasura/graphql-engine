const asyncState = {
  isRequesting: false,
  isUntracking: false,
  isDeleting: false,
  isError: false,
  isFetching: false,
  isUpdating: false,
  isFetchError: null,
  isPermissionSet: false,
  isPermissionDrop: false,
};

const functionData = {
  functionName: '',
  functionSchema: '',
  functionDefinition: '',
  configuration: {},
  permissions: {},
  setOffTable: '',
  setOffTableSchema: '',
  inputArgNames: [],
  inputArgTypes: [],
  ...asyncState,
};

export { functionData };
