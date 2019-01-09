const asyncState = {
  isRequesting: false,
  isUntracking: false,
  isDeleting: false,
  isError: false,
  isFetching: false,
  isFetchError: null,
};

const functionData = {
  functionName: '',
  functionSchema: '',
  functionDefinition: '',
  setOffTable: '',
  ...asyncState,
};

export { functionData };
