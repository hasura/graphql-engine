const defaultState = {
  triggerName: '',
  tableName: '',
  schemaName: 'public',
  tableListBySchema: [],
  operations: { insert: [], update: [], delete: [] },
  selectedOperations: { insert: false, update: false, delete: false },
  webhookURL: '',
  retryConf: null,
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
};

export default defaultState;
