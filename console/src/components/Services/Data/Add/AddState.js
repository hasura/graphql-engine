const defaultState = {
  tableName: null,
  columns: [{ name: '', type: '', nullable: false }],
  primaryKeys: [''],
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
};

export default defaultState;
