const defaultState = {
  triggerName: '',
  tableName: '',
  schemaName: 'public',
  tableListBySchema: [],
  operations: { insert: [], update: [], delete: [] },
  selectedOperations: {
    insert: false,
    update: false,
    delete: false,
    manual: false,
  },
  webhookURL: '',
  webhookUrlType: 'url',
  retryConf: {
    num_retries: 0,
    interval_sec: 10,
    timeout_sec: 60,
  },
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
  headers: [{ key: '', type: 'static', value: '' }],
};

export default defaultState;
