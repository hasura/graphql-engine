const defaultState = {
  triggerName: '',
  tableName: '',
  schemaName: 'public',
  operations: { insert: [], update: [], delete: [] },
  enableManual: false,
  selectedOperations: {
    insert: false,
    update: false,
    delete: false,
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
