import { MetadataTable } from '../../../hasura-metadata-types/source/table';
export const partiallyAppliedPermissionsData = {
  table: ['Album'],
  select_permissions: [
    {
      role: 'asdf',
      permission: {
        columns: ['Title'],
        filter: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
        allow_aggregations: true,
      },
    },
    {
      role: 'user',
      permission: {
        columns: { Title: true },
        filter: { AlbumId: { _eq: 'X-Hasura-User-Id' } },
        check: { AlbumId: { _eq: 'X-Hasura-User-Id' } },
      },
    },
  ],
  delete_permissions: [
    { role: 'asdf', permission: { filter: {} } },
    { role: 'testrole', permission: { filter: {} } },
    {
      role: 'user',
      permission: {
        filter: { AlbumId: { _eq: 'X-Hasura-User-I' } },
        check: { AlbumId: { _eq: 'X-Hasura-User-I' } },
        columns: {},
      },
    },
  ],
} as MetadataTable;

export const fullyAppliedPermissionsData = {
  table: ['Album'],
  insert_permissions: [
    {
      role: 'user',
      permission: {
        check: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
        columns: ['Title'],
      },
    },
  ],
  select_permissions: [
    {
      role: 'asdf',
      permission: {
        columns: ['Title'],
        filter: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
        allow_aggregations: true,
      },
    },
    {
      role: 'user',
      permission: {
        columns: { AlbumId: true, Title: true },
        filter: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
        check: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
      },
    },
  ],
  update_permissions: [
    {
      role: 'user',
      permission: {
        columns: { Title: true },
        filter: { Title: { _eq: 'X-Hasura-User-Id' } },
        check: { Title: { _eq: 'X-Hasura-User-Id' } },
      },
    },
  ],
  delete_permissions: [
    { role: 'asdf', permission: { filter: {} } },
    { role: 'testrole', permission: { filter: {} } },
    { role: 'user', permission: { filter: {}, check: {}, columns: {} } },
  ],
} as MetadataTable;

export const noAppliedPermissionsData = {
  table: ['Album'],
  select_permissions: [
    {
      role: 'asdf',
      permission: {
        columns: ['Title'],
        filter: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
        allow_aggregations: true,
      },
    },
  ],
  delete_permissions: [
    { role: 'asdf', permission: { filter: {} } },
    { role: 'testrole', permission: { filter: {} } },
  ],
} as MetadataTable;

export const alreadyExistingInsertPermissions = {
  table: ['Chinook', 'Artist'],
  insert_permissions: [
    {
      role: 'user',
      permission: {
        check: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
        columns: [],
        filter: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
      },
    },
  ],
  delete_permissions: [
    {
      role: 'user',
      permission: { filter: { ArtistId: { _eq: 1 }, columns: [] } },
    },
  ],
} as MetadataTable;
