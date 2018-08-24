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
        columns: ['*', { name: 'logs', columns: ['*'] }],
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

const defaultState = {
  currentTrigger: null,
  view: { ...defaultViewState },
  triggerList: [],
  listingTrigger: [],
  processedEvents: [],
  pendingEvents: [],
  schemaList: ['public'],
  currentSchema: 'public',
  accessKeyError: false,
  dataHeaders: {
    'Content-Type': 'application/json',
  },
};

export default defaultState;
export { defaultViewState, defaultCurFilter };
