import { Metadata } from '../../../hasura-metadata-types';

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

export const metadataTable = {
  name: ['Artist'],
  columns: [
    {
      name: 'ArtistId',
      type: 'number',
      nullable: false,
    },
    {
      name: 'Name',
      type: 'string',
      nullable: true,
    },
  ],
  primary_key: ['ArtistId'],
  description:
    'CREATE TABLE [Artist]\n(\n    [ArtistId] INTEGER  NOT NULL,\n    [Name] NVARCHAR(120),\n    CONSTRAINT [PK_Artist] PRIMARY KEY  ([ArtistId])\n)',
};

export const metadata: Metadata = {
  resource_version: 30,
  metadata: {
    inherited_roles: [],
    version: 3,
    sources: [
      {
        name: 'default',
        kind: 'postgres',
        tables: [
          { table: { schema: 'public', name: 'a_table' } },
          {
            table: { schema: 'public', name: 'user' },
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
      {
        name: 'sqlite',
        kind: 'sqlite',
        tables: [
          {
            table: ['Album'],
          },
          {
            table: ['Artist'],
            select_permissions: [
              {
                role: 'user',
                permission: {
                  columns: ['ArtistId', 'Name'],
                  filter: { ArtistId: { _eq: 'X-Hasura-User-Id' } },
                  allow_aggregations: true,
                },
              },
            ],
          },
        ],
        configuration: {
          template: null,
          timeout: null,
          value: {
            db: '/chinook.db',
            include_sqlite_meta_tables: false,
          },
        },
      },
    ],
  },
};

export const capabilitiesResponse = {
  capabilities: {
    comparisons: { subquery: { supports_relations: true } },
    data_schema: { supports_foreign_keys: true, supports_primary_keys: true },
    explain: {},
    mutations: {
      delete: {},
      insert: { supports_nested_inserts: true },
      update: {},
    },
    queries: {},
    raw: {},
    relationships: {},
    scalar_types: {
      DateTime: {
        comparison_operators: { _in_year: 'int' },
        graphql_type: 'String',
      },
      bool: {
        comparison_operators: {
          _and: 'bool',
          _nand: 'bool',
          _or: 'bool',
          _xor: 'bool',
        },
        graphql_type: 'Boolean',
      },
      decimal: {
        aggregate_functions: { max: 'decimal', min: 'decimal', sum: 'decimal' },
        comparison_operators: { _modulus_is_zero: 'decimal' },
        graphql_type: 'Float',
        update_column_operators: {
          dec: { argument_type: 'decimal' },
          inc: { argument_type: 'decimal' },
        },
      },
      number: {
        aggregate_functions: { max: 'number', min: 'number', sum: 'number' },
        comparison_operators: { _modulus_is_zero: 'number' },
        graphql_type: 'Float',
        update_column_operators: {
          dec: { argument_type: 'number' },
          inc: { argument_type: 'number' },
        },
      },
      string: {
        aggregate_functions: { max: 'string', min: 'string' },
        comparison_operators: { _glob: 'string', _like: 'string' },
        graphql_type: 'String',
      },
    },
  },
  config_schema_response: {
    config_schema: {
      nullable: false,
      properties: {
        DEBUG: {
          additionalProperties: true,
          description: 'For debugging.',
          nullable: true,
          type: 'object',
        },
        db: { description: 'The SQLite database file to use.', type: 'string' },
        explicit_main_schema: {
          default: false,
          description: "Prefix all tables with the 'main' schema",
          nullable: true,
          type: 'boolean',
        },
        include_sqlite_meta_tables: {
          description:
            'By default index tables, etc are not included, set this to true to include them.',
          nullable: true,
          type: 'boolean',
        },
        tables: {
          description:
            'List of tables to make available in the schema and for querying',
          items: { $ref: '#/other_schemas/TableName' },
          nullable: true,
          type: 'array',
        },
      },
      required: ['db'],
      type: 'object',
    },
    other_schemas: { TableName: { nullable: false, type: 'string' } },
  },
  display_name: 'Hasura SQLite',
  options: { uri: 'http://host.docker.internal:8100' },
};
