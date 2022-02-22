import { HasuraMetadataV3 } from '@/metadata/types';

export const schemaList = {
  result_type: 'TuplesOk',
  result: [['schema_name'], ['public'], ['default']],
};

export const query = {
  result_type: 'TuplesOk',
  result: [
    ['tables'],
    [
      '[{"table_schema":"public","table_name":"another_table","table_type":"TABLE","comment":"comment","columns":[{"comment": null, "data_type": "integer", "table_name": "another_table", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": "nextval(\'another_table_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": "column comment", "data_type": "text", "table_name": "another_table", "column_name": "name", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}, {"table_schema":"public","table_name":"users","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "users", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": "nextval(\'users_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "email", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 3}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "name", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "type", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 5}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "username", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 4}],"triggers":[],"view_info":null}]',
    ],
  ],
};

export const metadata = {
  resource_version: 30,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          { table: { schema: 'public', name: 'a_table' } },
          {
            table: { schema: 'public', name: 'users' },
            insert_permissions: [
              {
                role: 'user',
                permission: {
                  check: { id: { _eq: 1 } },
                  set: { id: 'x-hasura-as' },
                  columns: ['email', 'type'],
                  backend_only: false,
                },
              },
            ],
            select_permissions: [
              {
                role: 'user',
                permission: {
                  columns: ['email', 'id', 'type'],
                  filter: { id: { _eq: 1 } },
                  limit: 5,
                  allow_aggregations: true,
                },
              },
            ],
          },
        ],
        functions: [{ function: { schema: 'public', name: 'search_user2' } }],
        configuration: {
          connection_info: {
            use_prepared_statements: true,
            database_url: { from_env: 'HASURA_GRAPHQL_DATABASE_URL' },
            isolation_level: 'read-committed',
            pool_settings: {
              connection_lifetime: 600,
              retries: 1,
              idle_timeout: 180,
              max_connections: 50,
            },
          },
        },
      },
    ],
  } as HasuraMetadataV3,
};
