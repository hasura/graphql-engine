import { Metadata } from '../../../hasura-metadata-types';

export const mockMetadata: Metadata = {
  resource_version: 24,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'bikes',
        kind: 'mssql',
        tables: [
          { table: { name: 'brands', schema: 'production' } },
          { table: { name: 'categories', schema: 'production' } },
          { table: { name: 'customers', schema: 'sales' } },
          { table: { name: 'order_items', schema: 'sales' } },
          { table: { name: 'orders', schema: 'sales' } },
          { table: { name: 'products', schema: 'production' } },
          { table: { name: 'staffs', schema: 'sales' } },
          { table: { name: 'stocks', schema: 'production' } },
          { table: { name: 'stores', schema: 'sales' } },
        ],
        configuration: {
          connection_info: {
            connection_string:
              'DRIVER={ODBC Driver 17 for SQL Server};SERVER=host.docker.internal;DATABASE=bikes;Uid=SA;Pwd=reallyStrongPwd123',
            pool_settings: {
              idle_timeout: 5,
              total_max_connections: null,
            },
          },
        },
      },
      {
        name: 'chinook',
        kind: 'postgres',
        tables: [
          { table: { name: 'Album', schema: 'public' } },
          { table: { name: 'Artist', schema: 'public' } },
          { table: { name: 'Customer', schema: 'public' } },
          { table: { name: 'Employee', schema: 'public' } },
          { table: { name: 'Genre', schema: 'public' } },
          { table: { name: 'Invoice', schema: 'public' } },
          { table: { name: 'InvoiceLine', schema: 'public' } },
          { table: { name: 'MediaType', schema: 'public' } },
          { table: { name: 'Playlist', schema: 'public' } },
          { table: { name: 'PlaylistTrack', schema: 'public' } },
          { table: { name: 'Track', schema: 'public' } },
        ],
        configuration: {
          connection_info: {
            database_url:
              'postgres://postgres:test@host.docker.internal:6001/chinook',
            isolation_level: 'read-committed',
            use_prepared_statements: false,
          },
        },
      },
      {
        name: 'snowflake_test',
        kind: 'snowflake',
        tables: [
          { table: ['ALBUM'] },
          { table: ['ARTIST'] },
          { table: ['CUSTOMER'] },
          { table: ['EMPLOYEE'] },
          { table: ['GENRE'] },
          { table: ['INVOICE'] },
          { table: ['INVOICELINE'] },
          { table: ['MEDIATYPE'] },
          { table: ['PLAYLIST'] },
          { table: ['PLAYLISTTRACK'] },
          { table: ['TRACK'] },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            fully_qualify_all_names: false,
            jdbc_url: 'jdbc_url',
          },
        },
      },
    ],
    backend_configs: {
      dataconnector: {
        athena: { uri: 'http://host.docker.internal:8081/api/v1/athena' },
        mariadb: { uri: 'http://host.docker.internal:8081/api/v1/mariadb' },
        mysql8: { uri: 'http://host.docker.internal:8081/api/v1/mysql' },
        oracle: { uri: 'http://host.docker.internal:8081/api/v1/oracle' },
        snowflake: { uri: 'http://host.docker.internal:8081/api/v1/snowflake' },
      },
    },
  },
};

export const mockListSourceKindsResponse = {
  sources: [
    { available: true, builtin: true, display_name: 'pg', kind: 'pg' },
    {
      available: true,
      builtin: true,
      display_name: 'citus',
      kind: 'citus',
    },
    {
      available: true,
      builtin: true,
      display_name: 'cockroach',
      kind: 'cockroach',
    },
    {
      available: true,
      builtin: true,
      display_name: 'mssql',
      kind: 'mssql',
    },
    {
      available: true,
      builtin: true,
      display_name: 'bigquery',
      kind: 'bigquery',
    },
    {
      available: true,
      builtin: false,
      display_name: 'Amazon Athena',
      kind: 'athena',
      release_name: 'Beta',
    },
    {
      available: true,
      builtin: false,
      display_name: 'MariaDB',
      kind: 'mariadb',
      release_name: 'Beta',
    },
    {
      available: true,
      builtin: false,
      display_name: 'MySQL',
      kind: 'mysql8',
      release_name: 'Beta',
    },
    {
      available: true,
      builtin: false,
      display_name: 'Oracle',
      kind: 'oracle',
      release_name: 'Beta',
    },
    {
      available: true,
      builtin: false,
      display_name: 'Snowflake',
      kind: 'snowflake',
      release_name: 'Beta',
    },
  ],
};

export const mockInconsistentMetadata = {
  inconsistent_objects: [],
  is_consistent: true,
};
