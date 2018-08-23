const defaultState = {
  triggerName: null,
  columns: [],
  tableName: '',
  schemaName: 'public',
  tableListBySchema: [],
  operations: { insert: null, update: null, delete: null },
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
};

export default defaultState;
