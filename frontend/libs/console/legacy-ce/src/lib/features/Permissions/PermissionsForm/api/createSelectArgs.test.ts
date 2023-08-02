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
        permission: {
          columns: ['AlbumId', 'Title', 'ArtistId'],
          filter: { _not: { AlbumId: { _eq: 'X-Hasura-User-Id' } } },
          set: {},
          allow_aggregations: false,
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
        permission: { backend_only: false, filter: { Title: { _eq: 'Test' } } },
        source: 'Chinook',
      },
    },
  ]);
});

test('create insert args object from form data', () => {
  const result = createInsertArgs(insertArgs);

  expect(result).toEqual([
    {
      type: 'sqlagent_create_insert_permission',
      args: {
        table: ['Album'],
        role: 'user',
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
          backend_only: false,
        },
        source: 'Chinook',
      },
    },
  ]);
});
