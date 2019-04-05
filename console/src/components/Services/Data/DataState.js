const defaultCurFilter = {
  where: { $and: [{ '': { '': '' } }] },
  limit: 10,
  offset: 0,
  order_by: [{ column: '', type: 'asc', nulls: 'last' }],
};

const defaultViewState = {
  query: {
    columns: [],
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

const defaultPermissionsState = {
  table: '',
  role: '',
  query: '',
  custom_checked: false,
  newRole: '',
  limitEnabled: true,
  bulkSelect: [],
  applySamePermissions: [],
  tableSchemas: [],
};

const defaultSetState = {
  insert: {
    key: '',
    value: '',
  },
  update: {
    key: '',
    value: '',
  },
};
const defaultQueryPermissions = {
  insert: {
    check: {},
    allow_upsert: true,
    set: {},
    columns: [],
    localSet: [
      {
        ...defaultSetState.insert,
      },
    ],
    isSetConfigChecked: false,
  },
  select: {
    columns: [],
    filter: {},
    limit: null,
    allow_aggregations: false,
  },
  update: {
    columns: [],
    filter: {},
    set: {},
    localSet: [
      {
        ...defaultSetState.update,
      },
    ],
    isSetConfigChecked: false,
  },
  delete: {
    filter: {},
  },
};

const defaultModifyState = {
  activeEdit: {
    column: '',
    pk: null,
    fk: null,
    check: null,
    unique: null,
    index: null,
    rel: null,
    perm: '',
  },
  columnEdit: {},
  pkEdit: [''],
  pkModify: [''],
  fkModify: [
    {
      refTableName: '',
      colMappings: [{ '': '' }],
      onDelete: 'restrict',
      onUpdate: 'restrict',
    },
  ],
  relAdd: {
    isActive: true,
    name: '',
    tableName: '',
    isObjRel: null,
    lcol: [],
    rTable: null,
    rcol: [],
    manualColumns: [],
    isManualExpanded: false,
    manualRelInfo: {
      remoteSchema: '',
      tables: [],
    },
  },
  permissionsState: { ...defaultPermissionsState },
  prevPermissionState: { ...defaultPermissionsState },
  ongoingRequest: false,
  lastError: null,
  lastSuccess: null,
  viewDefinition: null,
  viewDefinitionError: null,
  tableCommentEdit: { enabled: false, editedValue: null },
};

const defaultState = {
  currentTable: null,
  view: { ...defaultViewState },
  modify: { ...defaultModifyState },
  insert: {
    clone: null,
    ongoingRequest: false,
    lastError: null,
    lastSuccess: null,
    isModalOpen: false,
  },
  update: {
    ongoingRequest: false,
    oldItem: null,
    pkClause: null,
    lastError: null,
    lastSuccess: null,
  },
  allSchemas: [],
  postgresFunctions: [],
  nonTrackablePostgresFunctions: [],
  trackedFunctions: [],
  listedFunctions: [],

  listingSchemas: [],
  untrackedSchemas: [],
  information_schema: [],
  tableComment: null,
  columnComments: {},
  untrackedRelations: [],
  schemaList: ['public'],
  currentSchema: 'public',
  adminSecretError: false,
  dataHeaders: {
    'content-type': 'application/json',
  },
};

export default defaultState;
export {
  defaultViewState,
  defaultCurFilter,
  defaultModifyState,
  defaultPermissionsState,
  defaultQueryPermissions,
  defaultSetState,
};
