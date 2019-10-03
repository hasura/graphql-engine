const defaultCurFilter = {
  where: { $and: [{ '': { '': '' } }] },
  limit: 10,
  offset: 0,
  order_by: [{ column: '', type: 'asc', nulls: 'last' }],
};

const defaultViewState = {
  query: {
    columns: [
      '*',
      {
        name: 'events',
        columns: [
          '*',
          { name: 'logs', columns: ['*'], order_by: ['-created_at'] },
        ],
      },
    ],
    limit: 10,
    offset: 0,
  },
  rows: [],
  expandedRow: '',
  count: 0,
  curFilter: defaultCurFilter,
  activePath: [],
  ongoingRequest: false,
  lastError: {},
  lastSuccess: {},
};

const defaultLogState = {
  query: {
    columns: [
      '*',
      {
        name: 'event',
        columns: ['*'],
      },
    ],
    limit: 10,
    offset: 0,
    order_by: ['-created_at'],
  },
  rows: [],
  expandedRow: '',
  count: 0,
  curFilter: defaultCurFilter,
  activePath: [],
  isLoadingOlder: false,
  isLoadingNewer: false,
  isOldAvailable: true,
  isNewAvailable: true,
  isModalOpen: false,
  redeliverEventId: null,
  eventInvocations: [],
  redeliverInvocationId: null,
  redeliverEventFailure: null,
  ongoingRequest: false,
  lastError: {},
  lastSuccess: {},
};

const defaultState = {
  currentTrigger: null,
  view: { ...defaultViewState },
  log: { ...defaultLogState },
  triggerList: [],
  listingTrigger: [],
  processedEvents: [],
  pendingEvents: [],
  runningEvents: [],
  eventLogs: [],
  schemaList: ['public'],
  currentSchema: 'public',
  adminSecretError: false,
  dataHeaders: {
    'content-type': 'application/json',
  },
};

export default defaultState;
export { defaultViewState, defaultLogState, defaultCurFilter };
