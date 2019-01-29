const defaultState = {
  triggerName: '',
  tableName: '',
  schemaName: 'public',
  tableListBySchema: [],
  operations: { insert: [], update: [], delete: [] },
  selectedOperations: { insert: false, update: false, delete: false },
  webhookURL: '',
  webhookUrlType: 'url',
  retryConf: {},
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
  headers: [{ key: '', type: 'static', value: '' }],
};

export default defaultState;
