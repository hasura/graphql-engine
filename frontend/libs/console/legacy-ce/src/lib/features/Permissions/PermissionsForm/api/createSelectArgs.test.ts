import { createInsertArgs } from './utils';
import {
  selectArgs,
  deleteArgs,
  insertArgs,
} from '../mocks/createPermissionsData.mock';
import { PermissionsSchema } from '../../schema';

const selectFormData = selectArgs.formData as Extract<
  PermissionsSchema,
  { queryType: 'select' }
>;

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

// Regression test for issue #10907: a select permission with named computed
// fields must serialize the names verbatim. `computed_fields` is an array of
// selected names (not a Record), so running Object.entries over it used to emit
// the array indices ("0", "1", ...) and the server rejected the metadata with
// `computed field "0" does not exist`.
test('serializes select computed_fields names, not array indices', () => {
  const result = createInsertArgs({
    ...selectArgs,
    formData: {
      ...selectFormData,
      computed_fields: ['full_name', 'total_spent', 'is_vip'],
    },
  });

  const createArg = result.find((arg) =>
    arg.type.endsWith('create_select_permission'),
  );
  expect(createArg?.args.permission.computed_fields).toEqual([
    'full_name',
    'total_spent',
    'is_vip',
  ]);
});

test('serializes an empty select computed_fields array as empty', () => {
  const result = createInsertArgs({
    ...selectArgs,
    formData: {
      ...selectFormData,
      computed_fields: [],
    },
  });

  const createArg = result.find((arg) =>
    arg.type.endsWith('create_select_permission'),
  );
  expect(createArg?.args.permission.computed_fields).toEqual([]);
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

test('skips the blank preset row the form keeps to add a preset with', () => {
  const formData = insertArgs.formData as Extract<
    PermissionsSchema,
    { queryType: 'insert' }
  >;
  const result = createInsertArgs({
    ...insertArgs,
    formData: {
      ...formData,
      presets: [
        { columnName: 'Title', presetType: 'static', columnValue: 'a' },
        { columnName: '', presetType: 'static', columnValue: '' },
      ],
    },
  });

  expect(result[0].args.permission.set).toEqual({ Title: 'a' });
});
