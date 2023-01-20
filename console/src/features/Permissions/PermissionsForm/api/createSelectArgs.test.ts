import { CreateInsertArgs, createInsertArgs } from './utils';

const selectArgs: CreateInsertArgs = {
  driver: 'postgres',
  dataSourceName: 'default',
  accessType: 'fullAccess',
  table: 'users',
  queryType: 'insert',
  role: 'user',
  formData: {
    queryType: 'select',

    filterType: 'none',

    filter: {},
    rowCount: '0',
    columns: {
      id: false,
      email: true,
      name: false,
      type: true,
      username: false,
    },
    aggregationEnabled: false,
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

test('create select args object from form data', () => {
  const result = createInsertArgs(selectArgs);

  expect(result).toEqual([
    {
      args: {
        role: 'user',
        source: 'default',
        table: 'users',
      },
      type: 'postgres_drop_insert_permission',
    },
    {
      args: {
        permission: {
          aggregation_enabled: false,
          columns: ['email', 'type'],
          filter: {},
        },
        role: 'user',
        source: 'default',
        table: 'users',
      },
      type: 'postgres_create_insert_permission',
    },
  ]);
});
