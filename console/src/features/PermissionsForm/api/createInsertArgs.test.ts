import { CreateInsertArgs, createInsertArgs } from './utils';

const insertArgs: CreateInsertArgs = {
  currentSource: 'postgres',
  dataSourceName: 'default',
  accessType: 'fullAccess',
  table: 'users',
  queryType: 'insert',
  role: 'user',
  formData: {
    checkType: 'custom',
    filterType: 'none',
    check: { id: { _eq: 1 } },
    filter: {},
    rowCount: '0',
    columns: {
      id: false,
      email: true,
      name: false,
      type: true,
      username: false,
    },
    presets: [
      {
        columnName: 'default',
        presetType: 'static',
        columnValue: '',
      },
    ],
    aggregationEnabled: false,
    backendOnly: false,
    clonePermissions: [
      {
        tableName: '',
        queryType: '',
        roleName: '',
      },
    ],
  },
  existingPermissions: [
    {
      table: 'users',
      role: 'user',
      queryType: 'insert',
    },
    {
      table: 'users',
      role: 'user',
      queryType: 'select',
    },
  ],
};

test('create insert args object from form data', () => {
  const result = createInsertArgs(insertArgs);

  expect(result).toEqual([
    {
      type: 'postgres_drop_insert_permission',
      args: {
        table: 'users',
        role: 'user',
        source: 'default',
      },
    },
    {
      type: 'postgres_create_insert_permission',
      args: {
        table: 'users',
        role: 'user',
        permission: {
          columns: ['email', 'type'],
          presets: {},
          computed_fields: [],
          backend_only: false,
          allow_aggregations: false,
          check: {
            id: {
              _eq: 1,
            },
          },
          filter: {},
        },
        source: 'default',
      },
    },
  ]);
});

const insertArgsWithClonePermissions: CreateInsertArgs = {
  currentSource: 'postgres',
  dataSourceName: 'default',
  accessType: 'fullAccess',
  table: 'users',
  queryType: 'insert',
  role: 'user',
  formData: {
    checkType: 'custom',
    filterType: 'none',
    check: { id: { _eq: 1 } },
    filter: {},
    rowCount: '0',
    columns: {
      id: false,
      email: true,
      name: false,
      type: true,
      username: false,
    },
    presets: [
      {
        columnName: 'default',
        presetType: 'static',
        columnValue: '',
      },
    ],
    aggregationEnabled: false,
    backendOnly: false,
    clonePermissions: [
      {
        tableName: 'a_table',
        queryType: 'select',
        roleName: 'user',
      },
      {
        tableName: '',
        queryType: '',
        roleName: '',
      },
    ],
  },
  existingPermissions: [
    {
      table: 'users',
      role: 'user',
      queryType: 'insert',
    },
    {
      table: 'users',
      role: 'user',
      queryType: 'select',
    },
  ],
};

test('create insert args object from form data with clone permissions', () => {
  const result = createInsertArgs(insertArgsWithClonePermissions);

  expect(result).toEqual([
    {
      type: 'postgres_drop_insert_permission',
      args: {
        table: 'users',
        role: 'user',
        source: 'default',
      },
    },
    {
      type: 'postgres_create_insert_permission',
      args: {
        table: 'users',
        role: 'user',
        permission: {
          columns: ['email', 'type'],
          presets: {},
          computed_fields: [],
          backend_only: false,
          allow_aggregations: false,
          check: {
            id: {
              _eq: 1,
            },
          },
          filter: {},
        },
        source: 'default',
      },
    },
    {
      type: 'postgres_create_select_permission',
      args: {
        table: 'a_table',
        role: 'user',
        permission: {
          columns: [],
          presets: {},
          computed_fields: [],
          backend_only: false,
          allow_aggregations: false,
          check: {
            id: {
              _eq: 1,
            },
          },
          filter: {},
        },
        source: 'default',
      },
    },
  ]);
});
