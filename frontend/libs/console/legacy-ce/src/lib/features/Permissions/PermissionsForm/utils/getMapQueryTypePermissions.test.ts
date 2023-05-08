import { getNonSelectedQueryTypePermissions } from './getMapQueryTypePermissions';
import {
  partiallyAppliedPermissionsData,
  fullyAppliedPermissionsData,
  noAppliedPermissionsData,
  alreadyExistingInsertPermissions,
} from './getMapQueryTypePermissions.mocks';

describe('getMapQueryTypePermissions should', () => {
  test('return existing permissions for table', () => {
    const result = getNonSelectedQueryTypePermissions(
      partiallyAppliedPermissionsData,
      'insert',
      'user'
    );
    expect(result).toEqual([
      {
        queryType: 'select',
        data: {
          columns: { Title: true },
          filter: { AlbumId: { _eq: 'X-Hasura-User-Id' } },
          check: { AlbumId: { _eq: 'X-Hasura-User-Id' } },
        },
      },
      {
        queryType: 'delete',
        data: {
          filter: { AlbumId: { _eq: 'X-Hasura-User-I' } },
          check: { AlbumId: { _eq: 'X-Hasura-User-I' } },
          columns: {},
        },
      },
    ]);
  });

  test('returns all existing permission except the current one', () => {
    const result = getNonSelectedQueryTypePermissions(
      fullyAppliedPermissionsData,
      'insert',
      'user'
    );
    expect(result).toEqual([
      {
        queryType: 'select',
        data: {
          columns: { AlbumId: true, Title: true },
          filter: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
          check: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
        },
      },
      {
        queryType: 'pre_update',
        data: {
          columns: { Title: true },
          filter: { Title: { _eq: 'X-Hasura-User-Id' } },
          check: { Title: { _eq: 'X-Hasura-User-Id' } },
        },
      },
      {
        queryType: 'post_update',
        data: {
          columns: { Title: true },
          filter: { Title: { _eq: 'X-Hasura-User-Id' } },
          check: { Title: { _eq: 'X-Hasura-User-Id' } },
        },
      },
      { queryType: 'delete', data: { filter: {}, check: {}, columns: {} } },
    ]);
  });

  test('return empty array when no permissions have been applied for table', () => {
    const result = getNonSelectedQueryTypePermissions(
      noAppliedPermissionsData,
      'insert',
      'user'
    );

    expect(result).toEqual([]);
  });

  test('returns filters values for select permissions cloning', () => {
    const result = getNonSelectedQueryTypePermissions(
      alreadyExistingInsertPermissions,
      'select',
      'user'
    );

    expect(result).toEqual([
      {
        queryType: 'insert',
        data: {
          check: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
          columns: {},
          filter: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
        },
      },
      {
        queryType: 'delete',
        data: { filter: { ArtistId: { _eq: 1 }, columns: [] }, columns: {} },
      },
    ]);
  });
});
