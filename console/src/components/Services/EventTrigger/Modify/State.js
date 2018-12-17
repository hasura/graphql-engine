const defaultState = {
  definition: {},
  webhookURL: '',
  webhookUrlType: 'url',
  retryConf: {
    numRetrys: 0,
    retryInterval: 10,
  },
  ongoingRequest: false,
  lastError: null,
  internalError: null,
  lastSuccess: null,
  headers: [{ key: '', type: 'static', value: '' }],
};

export default defaultState;
