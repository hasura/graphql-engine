import { dataSource } from '../../../dataSources';

const defaultCurFilter = {
  where: { $and: [{ '': { $eq: '' } }] },
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
  manualTriggers: [],
  estimatedCount: 0,
  isCountEstimated: 0,
};

const defaultPermissionsState = {
  table: '',
  role: '',
  query: '',
  custom_checked: {
    check: false,
    filter: false,
  },
  newRole: '',
  limitEnabled: true,
  bulkSelect: [],
  applySamePermissions: [],
  isEditing: false,
  inconsistentInhertiedRole: null,
};

const defaultQueryPermissions = {
  insert: {
    check: {},
    allow_upsert: true,
    backend_only: false,
    set: {},
    columns: [],
  },
  select: {
    columns: [],
    computed_fields: [],
    backend_only: false,
    filter: {},
    limit: null,
    allow_aggregations: false,
  },
  update: {
    columns: [],
    filter: {},
    backend_only: false,
    set: {},
  },
  delete: {
    backend_only: false,
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
  tableEnum: {
    loading: false,
  },
  columnEdit: {},
  pkEdit: [''],
  pkModify: [''],
  fkModify: [
    {
      refSchemaName: '',
      refTableName: '',
      colMappings: [{ '': '' }],
      onDelete: dataSource?.violationActions?.[0] ?? '',
      onUpdate: dataSource?.violationActions?.[0] ?? '',
    },
  ],
  checkConstraintsModify: [],
  uniqueKeyModify: [[]],
  relAdd: {
    isActive: true,
    name: '',
    lTable: null,
    lSchema: null,
    isObjRel: null,
    lcol: [],
    rTable: null,
    rSchema: null,
    rcol: [],
    isUnique: false,
    isPrimary: false,
  },
  manualRelAdd: {
    relName: '',
    relType: '',
    rSchema: '',
    rTable: '',
    colMappings: [{ column: '', refColumn: '' }],
    isToggled: false,
  },
  remoteRelationships: {
    remoteSchema: {},
  },
  custom_name: '',
  rootFieldsEdit: {
    select: '',
    select_by_pk: '',
    select_aggregate: '',
    insert: '',
    insert_one: '',
    update: '',
    update_by_pk: '',
    delete: '',
    delete_by_pk: '',
  },
  permissionsState: defaultPermissionsState,
  prevPermissionState: defaultPermissionsState,
  ongoingRequest: false,
  lastError: null,
  lastSuccess: null,
  viewDefinition: null,
  viewDefinitionError: null,
  viewDefSql: '',
  tableCommentEdit: { enabled: false, editedValue: null },
  alterColumnOptions: [], // Store supported implicit column -> column casts
  alterColumnOptionsFetchErr: null,
};

const defaultState = {
  schemaFilter: [],
  tableFilter: {},
  columnDataTypes: [], // To store list of column types supported by postgres
  columnDataTypeInfoErr: null,
  columnDefaultFunctions: {},
  columnTypeCasts: {},
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
  listingSchemas: [],
  untrackedRelations: [],
  schemaList: [],
  currentSchema: '',
  currentDataSource: '',
  adminSecretError: false,
  dataHeaders: {
    'content-type': 'application/json',
  },
  dbConnection: {
    envVar: '',
    dbURL: '',
    dbName: '',
  },
  allSourcesSchemas: {},
};

export default defaultState;
export {
  defaultViewState,
  defaultCurFilter,
  defaultModifyState,
  defaultPermissionsState,
  defaultQueryPermissions,
};
