import { updateTablePermission } from '../cache';
import { metadata } from '../../mocks/dataStubs';

const data = {
  type: 'bulk',
  source: 'default',
  resource_version: 30,
  args: [
    {
      type: 'pg_create_insert_permission',
      args: {
        table: {
          name: 'users',
          schema: 'public',
        },
        role: 'user',
        permission: {
          columns: ['email', 'type'],
          presets: {},
          computed_fields: [],
          backend_only: false,
          limit: 0,
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
  ],
};

test('update metadata in cache', () => {
  const result = updateTablePermission({
    key: 'insert_permissions',
    tableName: 'users',
    roleName: 'user',
    metadata,
    data,
  });

  expect(result).toMatchSnapshot();
});
