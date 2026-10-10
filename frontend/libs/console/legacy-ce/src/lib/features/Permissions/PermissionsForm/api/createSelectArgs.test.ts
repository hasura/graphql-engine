import { createInsertArgs } from './utils';
import {
  selectArgs,
  deleteArgs,
  insertArgs,
} from '../mocks/createPermissionsData.mock';

test('create select args object from form data', () => {
  const result = createInsertArgs(selectArgs);
  expect(result).toEqual([
    {
      type: 'sqlagent_drop_select_permission',
      args: { table: ['Album'], role: 'user', source: 'Chinook' },
    },
    {
      type: 'sqlagent_create_select_permission',
      args: {
        table: ['Album'],
        role: 'user',
        comment: '',
        permission: {
          columns: ['AlbumId', 'Title', 'ArtistId'],
          filter: { _not: { AlbumId: { _eq: 'X-Hasura-User-Id' } } },
          set: {},
          allow_aggregations: false,
          computed_fields: [],
          // The mock's `formData.rowCount` is the explicit string '0' (the
          // user typed 0 into the "Limit number of rows" field), which is
          // meaningfully different from leaving the field blank. createSelectObject
          // (api/utils.ts) treats `rowCount === '0'` as "limit to 0 rows" on
          // purpose, not as "no limit" -- see createPermissionsData.mock.ts.
          limit: 0,
        },
        source: 'Chinook',
      },
    },
  ]);
});

test('create delete args object from form data', () => {
  const result = createInsertArgs(deleteArgs);

  expect(result).toEqual([
    {
      type: 'sqlagent_drop_delete_permission',
      args: { table: ['Album'], role: 'user', source: 'Chinook' },
    },
    {
      type: 'sqlagent_create_delete_permission',
      args: {
        table: ['Album'],
        role: 'user',
        comment: '',
        permission: { backend_only: false, filter: { Title: { _eq: 'Test' } } },
        source: 'Chinook',
      },
    },
  ]);
});

test('create delete args object keeps input validation', () => {
  const result = createInsertArgs({
    ...deleteArgs,
    formData: {
      ...deleteArgs.formData,
      validateInput: {
        enabled: true,
        type: 'http',
        definition: {
          url: 'http://host.docker.internal',
          forward_client_headers: true,
          headers: [],
          timeout: 40,
        },
      },
    },
  });

  expect(result[1].args.permission).toEqual({
    backend_only: false,
    filter: { Title: { _eq: 'Test' } },
    validate_input: {
      type: 'http',
      definition: {
        url: 'http://host.docker.internal',
        forward_client_headers: true,
        headers: [],
        timeout: 40,
      },
    },
  });
});

test('create insert args object from form data', () => {
  const result = createInsertArgs(insertArgs);

  expect(result).toEqual([
    {
      type: 'sqlagent_create_insert_permission',
      args: {
        table: ['Album'],
        role: 'user',
        comment: '',
        permission: {
          columns: [],
          check: {
            _and: [
              {},
              { AlbumId: { _eq: '1337' } },
              { _not: { ArtistId: { _eq: '1338' } } },
            ],
          },
          allow_upsert: true,
          set: {},
          validate_input: undefined,
          backend_only: false,
        },
        source: 'Chinook',
      },
    },
  ]);
});
