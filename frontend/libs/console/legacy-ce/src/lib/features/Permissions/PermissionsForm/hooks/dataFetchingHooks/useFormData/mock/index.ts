export const useFormDataCreateDefaultValuesMock = {
  queryType: 'select' as const,
  roleName: 'asdf',
  dataSourceName: 'bigquery_test1',
  metadata: {
    resource_version: 485,
    metadata: {
      version: 3 as const,
      sources: [
        {
          name: 'default',
          kind: 'postgres',
          tables: [
            {
              table: { name: 'stuff', schema: 'erik' },
              insert_permissions: [
                {
                  role: 'sdfsf',
                  permission: {
                    check: { id: { _eq: 'X-Hasura-User-Id' } },
                    columns: [],
                  },
                },
              ],
            },
            {
              table: { name: 'class', schema: 'public' },
              configuration: {
                column_config: {},
                custom_column_names: {},
                custom_name: 'custom_class',
                custom_root_fields: {},
              },
              object_relationships: [
                {
                  name: 'object_relationship',
                  using: {
                    manual_configuration: {
                      column_mapping: { id: 'id' },
                      insertion_order: null,
                      remote_table: { name: 'identitytest', schema: 'public' },
                    },
                  },
                },
              ],
              array_relationships: [
                {
                  name: 'class_students',
                  using: {
                    foreign_key_constraint_on: {
                      column: 'class',
                      table: { name: 'class_student', schema: 'public' },
                    },
                  },
                },
              ],
              insert_permissions: [
                {
                  role: 'testrole',
                  permission: {
                    check: { id: { _eq: 'X-Hasura-User-Id' } },
                    columns: ['id'],
                  },
                },
                {
                  role: 'user',
                  permission: {
                    check: { id: { _eq: 'X-Hasura-User-Id' } },
                    columns: ['id'],
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'asdf',
                  permission: {
                    columns: ['id', 'teacher'],
                    filter: {
                      _exists: {
                        _table: { name: 'testing', schema: 'public' },
                        _where: {
                          _exists: {
                            _table: { name: 'testing', schema: 'public' },
                            _where: {},
                          },
                        },
                      },
                    },
                  },
                },
                {
                  role: 'new',
                  permission: {
                    columns: ['id', 'teacher'],
                    filter: {
                      _exists: {
                        _table: { name: 'testing', schema: 'public' },
                        _where: { id: { _eq: 'X-Hasura-User-I' } },
                      },
                    },
                  },
                },
                {
                  role: 'sdfsf',
                  permission: {
                    columns: ['id'],
                    filter: {
                      _exists: {
                        _table: { name: 'class_student', schema: 'public' },
                        _where: { student_id: { _eq: 'X-Hasura-User-Id' } },
                      },
                    },
                    query_root_fields: ['select', 'select_by_pk'],
                    subscription_root_fields: ['select', 'select_by_pk'],
                  },
                },
                {
                  role: 'testrole',
                  permission: {
                    columns: ['id', 'teacher'],
                    filter: {
                      class_students: { class: { _eq: 'X-Hasura-User-Id' } },
                    },
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: ['id'],
                    filter: {
                      _exists: {
                        _table: { name: 'class', schema: 'public' },
                        _where: { id: { _eq: 'X-Hasura-User-Id' } },
                      },
                    },
                    allow_aggregations: true,
                    query_root_fields: [
                      'select_aggregate',
                      'select',
                      'select_by_pk',
                    ],
                    subscription_root_fields: [
                      'select_aggregate',
                      'select',
                      'select_by_pk',
                    ],
                  },
                },
              ],
            },
            {
              table: { name: 'class_student', schema: 'public' },
              object_relationships: [
                {
                  name: 'classByClass',
                  using: { foreign_key_constraint_on: 'class' },
                },
                {
                  name: 'user',
                  using: { foreign_key_constraint_on: 'student_id' },
                },
              ],
              select_permissions: [
                {
                  role: 'new',
                  permission: {
                    columns: ['class', 'id'],
                    filter: {},
                    allow_aggregations: true,
                    query_root_fields: [
                      'select_aggregate',
                      'select',
                      'select_by_pk',
                    ],
                    subscription_root_fields: [
                      'select_aggregate',
                      'select',
                      'select_by_pk',
                    ],
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: ['class', 'id', 'student_id'],
                    filter: {},
                    allow_aggregations: true,
                  },
                },
              ],
            },
            { table: { name: 'identitytest', schema: 'public' } },
            {
              table: { name: 'testing', schema: 'public' },
              insert_permissions: [
                {
                  role: 'user',
                  permission: {
                    check: {},
                    columns: ['id', 'metadata', 'deleted_at'],
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'asdf',
                  permission: {
                    columns: ['id', 'metadata', 'deleted_at'],
                    filter: {},
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: ['deleted_at', 'id', 'metadata'],
                    filter: { deleted_at: { _is_null: true } },
                    allow_aggregations: true,
                  },
                },
              ],
              update_permissions: [
                {
                  role: 'user',
                  permission: {
                    columns: ['id', 'metadata'],
                    filter: {},
                    check: {},
                  },
                },
              ],
            },
            { table: { name: 'testtest', schema: 'public' } },
            {
              table: { name: 'user', schema: 'public' },
              array_relationships: [
                {
                  name: 'class_students',
                  using: {
                    foreign_key_constraint_on: {
                      column: 'student_id',
                      table: { name: 'class_student', schema: 'public' },
                    },
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'sdfsf',
                  permission: {
                    columns: ['id', 'name', 'deleted_at'],
                    filter: { _or: [] },
                    query_root_fields: ['select', 'select_by_pk'],
                    subscription_root_fields: ['select', 'select_by_pk'],
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: ['deleted_at', 'id', 'name'],
                    filter: { deleted_at: { _is_null: true } },
                    query_root_fields: ['select', 'select_by_pk'],
                    subscription_root_fields: ['select', 'select_by_pk'],
                  },
                },
              ],
            },
          ],
        },
        {
          name: 'Chinook',
          kind: 'sqlagent',
          tables: [
            {
              table: ['Album'],
              configuration: {
                column_config: {},
                custom_column_names: {},
                custom_name: 'custom_album',
                custom_root_fields: {},
              },
              object_relationships: [
                {
                  name: 'test_rel',
                  using: {
                    manual_configuration: {
                      column_mapping: { Title: 'ArtistId' },
                      insertion_order: null,
                      remote_table: ['Album'],
                    },
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'asdf',
                  permission: {
                    columns: ['AlbumId'],
                    filter: { _or: [{ AlbumId: { _eq: 'X-Hasura-User-Id' } }] },
                  },
                },
                {
                  role: 'new',
                  permission: {
                    columns: ['AlbumId'],
                    filter: { _or: [{ AlbumId: { _eq: 'X-Hasura-User-Id' } }] },
                  },
                },
                {
                  role: 'sdfsf',
                  permission: {
                    columns: ['AlbumId', 'Title', 'ArtistId'],
                    filter: {
                      _and: [{ AlbumId: { _eq: 'X-Hasura-User-Id' } }],
                    },
                  },
                },
                {
                  role: 'testrole',
                  permission: {
                    columns: ['AlbumId', 'Title'],
                    filter: { _and: [{ AlbumId: { _eq: 'X-Hasura-User' } }] },
                  },
                },
              ],
            },
            {
              table: ['Artist'],
              configuration: {
                column_config: {},
                custom_column_names: {},
                custom_name: 'silly_artist',
                custom_root_fields: {},
              },
              select_permissions: [
                {
                  role: 'testrole',
                  permission: {
                    columns: [],
                    filter: {
                      _exists: {
                        _table: ['Album'],
                        _where: { AlbumId: { _eq: 'X-Hasura-User-Id' } },
                      },
                    },
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
              explicit_main_schema: false,
              include_sqlite_meta_tables: false,
              tables: ['Artist', 'Album'],
            },
          },
        },
        {
          name: 'bigquery_test1',
          kind: 'bigquery',
          tables: [
            {
              table: { dataset: 'bigquery_sample', name: 'sample_table' },
              object_relationships: [
                {
                  name: 'bq_test_relation',
                  using: {
                    manual_configuration: {
                      column_mapping: { Period: 'STATUS' },
                      insertion_order: null,
                      remote_table: {
                        dataset: 'bigquery_sample',
                        name: 'sample_table',
                      },
                    },
                  },
                },
                {
                  name: 'test_2',
                  using: {
                    manual_configuration: {
                      column_mapping: { Period: 'Series_title_2' },
                      insertion_order: null,
                      remote_table: {
                        dataset: 'bigquery_sample',
                        name: 'sample_table',
                      },
                    },
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'asdf',
                  permission: {
                    columns: [],
                    filter: { _not: { Data_value: { _eq: 1337 } } },
                  },
                },
                {
                  role: 'new',
                  permission: {
                    columns: ['Series_reference', 'Period'],
                    filter: {
                      _and: [
                        { Data_value: { _eq: 'X-Hasura-User-Id' } },
                        { Group: { _eq: 'X-Hasura-User-Id' } },
                      ],
                    },
                    allow_aggregations: true,
                    query_root_fields: [],
                    subscription_root_fields: [],
                  },
                },
                {
                  role: 'sdfsf',
                  permission: {
                    columns: [
                      'Series_reference',
                      'Period',
                      'Data_value',
                      'Suppressed',
                      'STATUS',
                      'UNITS',
                      'Magnitude',
                      'Subject',
                      'Group',
                      'Series_title_1',
                      'Series_title_2',
                      'Series_title_3',
                      'Series_title_4',
                      'Series_title_5',
                    ],
                    filter: {
                      _and: [{ Series_reference: { _eq: 'X-Hasura-User-Id' } }],
                    },
                  },
                },
                {
                  role: 'testrole',
                  permission: {
                    columns: [],
                    filter: { Magnitude: { _eq: '123' } },
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: [
                      'Series_reference',
                      'Period',
                      'Data_value',
                      'Suppressed',
                      'STATUS',
                      'UNITS',
                      'Magnitude',
                      'Subject',
                      'Group',
                      'Series_title_1',
                      'Series_title_2',
                      'Series_title_3',
                      'Series_title_4',
                      'Series_title_5',
                    ],
                    filter: {},
                  },
                },
              ],
            },
          ],
        },
        {
          name: 'cockroach',
          kind: 'cockroach',
          tables: [
            { table: { name: 'table1', schema: 'public' } },
            { table: { name: 'table2', schema: 'public' } },
          ],
          configuration: {
            connection_info: {
              database_url:
                'postgresql://root@host.docker.internal:26257/defaultdb',
              isolation_level: 'read-committed',
              use_prepared_statements: false,
            },
          },
        },
      ],
      actions: [
        {
          name: 'actionName',
          definition: {
            handler: 'http://host.docker.internal:3000',
            output_type: 'SampleOutput',
            arguments: [{ name: 'arg1', type: 'SampleInput!' }],
            type: 'mutation',
            kind: 'synchronous',
          },
          comment: 'test',
          permissions: [{ role: 'user' }],
        },
      ],
      custom_types: {
        input_objects: [
          {
            name: 'SampleInput',
            fields: [
              { name: 'username', type: 'String!' },
              { name: 'password', type: 'String!' },
            ],
          },
        ],
        objects: [
          {
            name: 'SampleOutput',
            fields: [{ name: 'accessToken', type: 'String!' }],
          },
        ],
      },
      backend_configs: {
        dataconnector: {
          sqlagent: { uri: 'http://host.docker.internal:8100' },
        },
      },
    },
  },
  table: { dataset: 'bigquery_sample', name: 'sample_table' },
  tableColumns: [
    {
      name: 'Series_reference',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Period',
      dataType: 'FLOAT64',
      consoleDataType: 'number',
      nullable: false,
    },
    {
      name: 'Data_value',
      dataType: 'FLOAT64',
      consoleDataType: 'number',
      nullable: false,
    },
    {
      name: 'Suppressed',
      dataType: 'BOOL',
      consoleDataType: 'boolean',
      nullable: false,
    },
    {
      name: 'STATUS',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'UNITS',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Magnitude',
      dataType: 'INT64',
      consoleDataType: 'number',
      nullable: false,
    },
    {
      name: 'Subject',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Group',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_1',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_2',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_3',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_4',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_5',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
  ],
  defaultQueryRoot: 'bigquery_sample_sample_table',
  metadataSource: {
    name: 'bigquery_test1',
    kind: 'bigquery',
    tables: [
      {
        table: { dataset: 'bigquery_sample', name: 'sample_table' },
        object_relationships: [
          {
            name: 'bq_test_relation',
            using: {
              manual_configuration: {
                column_mapping: { Period: 'STATUS' },
                insertion_order: null,
                remote_table: {
                  dataset: 'bigquery_sample',
                  name: 'sample_table',
                },
              },
            },
          },
          {
            name: 'test_2',
            using: {
              manual_configuration: {
                column_mapping: { Period: 'Series_title_2' },
                insertion_order: null,
                remote_table: {
                  dataset: 'bigquery_sample',
                  name: 'sample_table',
                },
              },
            },
          },
        ],
        select_permissions: [
          {
            role: 'asdf',
            permission: {
              columns: [],
              filter: { _not: { Data_value: { _eq: 1337 } } },
            },
          },
          {
            role: 'new',
            permission: {
              columns: ['Series_reference', 'Period'],
              filter: {
                _and: [
                  { Data_value: { _eq: 'X-Hasura-User-Id' } },
                  { Group: { _eq: 'X-Hasura-User-Id' } },
                ],
              },
              allow_aggregations: true,
              query_root_fields: [],
              subscription_root_fields: [],
            },
          },
          {
            role: 'sdfsf',
            permission: {
              columns: [
                'Series_reference',
                'Period',
                'Data_value',
                'Suppressed',
                'STATUS',
                'UNITS',
                'Magnitude',
                'Subject',
                'Group',
                'Series_title_1',
                'Series_title_2',
                'Series_title_3',
                'Series_title_4',
                'Series_title_5',
              ],
              filter: {
                _and: [{ Series_reference: { _eq: 'X-Hasura-User-Id' } }],
              },
            },
          },
          {
            role: 'testrole',
            permission: { columns: [], filter: { Magnitude: { _eq: '123' } } },
          },
          {
            role: 'user',
            permission: {
              columns: [
                'Series_reference',
                'Period',
                'Data_value',
                'Suppressed',
                'STATUS',
                'UNITS',
                'Magnitude',
                'Subject',
                'Group',
                'Series_title_1',
                'Series_title_2',
                'Series_title_3',
                'Series_title_4',
                'Series_title_5',
              ],
              filter: {},
            },
          },
        ],
      },
    ],
  },
  supportedOperators: [
    { name: 'equals', value: '_eq' },
    { name: 'not equals', value: '_neq' },
    { name: 'in', value: '_in', defaultValue: '[]' },
    { name: 'nin', value: '_nin', defaultValue: '[]' },
    { name: '>', value: '_gt' },
    { name: '<', value: '_lt' },
    { name: '>=', value: '_gte' },
    { name: '<=', value: '_lte' },
    { name: 'like', value: '_like', defaultValue: '%%' },
    { name: 'not like', value: '_nlike', defaultValue: '%%' },
  ],
} as any;

export const createFormDataMock = {
  dataSourceName: 'bigquery_test1',
  table: { dataset: 'bigquery_sample', name: 'sample_table' },
  metadata: {
    resource_version: 485,
    metadata: {
      version: 3 as const,
      sources: [
        {
          name: 'default',
          kind: 'postgres',
          tables: [
            {
              table: { name: 'stuff', schema: 'erik' },
              insert_permissions: [
                {
                  role: 'sdfsf',
                  permission: {
                    check: { id: { _eq: 'X-Hasura-User-Id' } },
                    columns: [],
                  },
                },
              ],
            },
            {
              table: { name: 'class', schema: 'public' },
              configuration: {
                column_config: {},
                custom_column_names: {},
                custom_name: 'custom_class',
                custom_root_fields: {},
              },
              object_relationships: [
                {
                  name: 'object_relationship',
                  using: {
                    manual_configuration: {
                      column_mapping: { id: 'id' },
                      insertion_order: null,
                      remote_table: { name: 'identitytest', schema: 'public' },
                    },
                  },
                },
              ],
              array_relationships: [
                {
                  name: 'class_students',
                  using: {
                    foreign_key_constraint_on: {
                      column: 'class',
                      table: { name: 'class_student', schema: 'public' },
                    },
                  },
                },
              ],
              insert_permissions: [
                {
                  role: 'testrole',
                  permission: {
                    check: { id: { _eq: 'X-Hasura-User-Id' } },
                    columns: ['id'],
                  },
                },
                {
                  role: 'user',
                  permission: {
                    check: { id: { _eq: 'X-Hasura-User-Id' } },
                    columns: ['id'],
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'asdf',
                  permission: {
                    columns: ['id', 'teacher'],
                    filter: {
                      _exists: {
                        _table: { name: 'testing', schema: 'public' },
                        _where: {
                          _exists: {
                            _table: { name: 'testing', schema: 'public' },
                            _where: {},
                          },
                        },
                      },
                    },
                  },
                },
                {
                  role: 'new',
                  permission: {
                    columns: ['id', 'teacher'],
                    filter: {
                      _exists: {
                        _table: { name: 'testing', schema: 'public' },
                        _where: { id: { _eq: 'X-Hasura-User-I' } },
                      },
                    },
                  },
                },
                {
                  role: 'sdfsf',
                  permission: {
                    columns: ['id'],
                    filter: {
                      _exists: {
                        _table: { name: 'class_student', schema: 'public' },
                        _where: { student_id: { _eq: 'X-Hasura-User-Id' } },
                      },
                    },
                    query_root_fields: ['select', 'select_by_pk'],
                    subscription_root_fields: ['select', 'select_by_pk'],
                  },
                },
                {
                  role: 'testrole',
                  permission: {
                    columns: ['id', 'teacher'],
                    filter: {
                      class_students: { class: { _eq: 'X-Hasura-User-Id' } },
                    },
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: ['id'],
                    filter: {
                      _exists: {
                        _table: { name: 'class', schema: 'public' },
                        _where: { id: { _eq: 'X-Hasura-User-Id' } },
                      },
                    },
                    allow_aggregations: true,
                    query_root_fields: [
                      'select_aggregate',
                      'select',
                      'select_by_pk',
                    ],
                    subscription_root_fields: [
                      'select_aggregate',
                      'select',
                      'select_by_pk',
                    ],
                  },
                },
              ],
            },
            {
              table: { name: 'class_student', schema: 'public' },
              object_relationships: [
                {
                  name: 'classByClass',
                  using: { foreign_key_constraint_on: 'class' },
                },
                {
                  name: 'user',
                  using: { foreign_key_constraint_on: 'student_id' },
                },
              ],
              select_permissions: [
                {
                  role: 'new',
                  permission: {
                    columns: ['class', 'id'],
                    filter: {},
                    allow_aggregations: true,
                    query_root_fields: [
                      'select_aggregate',
                      'select',
                      'select_by_pk',
                    ],
                    subscription_root_fields: [
                      'select_aggregate',
                      'select',
                      'select_by_pk',
                    ],
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: ['class', 'id', 'student_id'],
                    filter: {},
                    allow_aggregations: true,
                  },
                },
              ],
            },
            { table: { name: 'identitytest', schema: 'public' } },
            {
              table: { name: 'testing', schema: 'public' },
              insert_permissions: [
                {
                  role: 'user',
                  permission: {
                    check: {},
                    columns: ['id', 'metadata', 'deleted_at'],
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'asdf',
                  permission: {
                    columns: ['id', 'metadata', 'deleted_at'],
                    filter: {},
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: ['deleted_at', 'id', 'metadata'],
                    filter: { deleted_at: { _is_null: true } },
                    allow_aggregations: true,
                  },
                },
              ],
              update_permissions: [
                {
                  role: 'user',
                  permission: {
                    columns: ['id', 'metadata'],
                    filter: {},
                    check: {},
                  },
                },
              ],
            },
            { table: { name: 'testtest', schema: 'public' } },
            {
              table: { name: 'user', schema: 'public' },
              array_relationships: [
                {
                  name: 'class_students',
                  using: {
                    foreign_key_constraint_on: {
                      column: 'student_id',
                      table: { name: 'class_student', schema: 'public' },
                    },
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'sdfsf',
                  permission: {
                    columns: ['id', 'name', 'deleted_at'],
                    filter: { _or: [] },
                    query_root_fields: ['select', 'select_by_pk'],
                    subscription_root_fields: ['select', 'select_by_pk'],
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: ['deleted_at', 'id', 'name'],
                    filter: { deleted_at: { _is_null: true } },
                    query_root_fields: ['select', 'select_by_pk'],
                    subscription_root_fields: ['select', 'select_by_pk'],
                  },
                },
              ],
            },
          ],
          configuration: {
            connection_info: {
              database_url: { from_env: 'HASURA_GRAPHQL_DATABASE_URL' },
              isolation_level: 'read-committed',
              pool_settings: {
                connection_lifetime: 600,
                idle_timeout: 180,
                max_connections: 50,
                retries: 1,
              },
              use_prepared_statements: true,
            },
          },
        },
        {
          name: 'Chinook',
          kind: 'sqlagent',
          tables: [
            {
              table: ['Album'],
              configuration: {
                column_config: {},
                custom_column_names: {},
                custom_name: 'custom_album',
                custom_root_fields: {},
              },
              object_relationships: [
                {
                  name: 'test_rel',
                  using: {
                    manual_configuration: {
                      column_mapping: { Title: 'ArtistId' },
                      insertion_order: null,
                      remote_table: ['Album'],
                    },
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'asdf',
                  permission: {
                    columns: ['AlbumId'],
                    filter: {
                      _or: [{ AlbumId: { _eq: 'X-Hasura-User-Id' } }],
                    },
                  },
                },
                {
                  role: 'new',
                  permission: {
                    columns: ['AlbumId'],
                    filter: {
                      _or: [{ AlbumId: { _eq: 'X-Hasura-User-Id' } }],
                    },
                  },
                },
                {
                  role: 'sdfsf',
                  permission: {
                    columns: ['AlbumId', 'Title', 'ArtistId'],
                    filter: {
                      _and: [{ AlbumId: { _eq: 'X-Hasura-User-Id' } }],
                    },
                  },
                },
                {
                  role: 'testrole',
                  permission: {
                    columns: ['AlbumId', 'Title'],
                    filter: { _and: [{ AlbumId: { _eq: 'X-Hasura-User' } }] },
                  },
                },
              ],
            },
            {
              table: ['Artist'],
              configuration: {
                column_config: {},
                custom_column_names: {},
                custom_name: 'silly_artist',
                custom_root_fields: {},
              },
              select_permissions: [
                {
                  role: 'testrole',
                  permission: {
                    columns: [],
                    filter: {
                      _exists: {
                        _table: ['Album'],
                        _where: { AlbumId: { _eq: 'X-Hasura-User-Id' } },
                      },
                    },
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
              explicit_main_schema: false,
              include_sqlite_meta_tables: false,
              tables: ['Artist', 'Album'],
            },
          },
        },
        {
          name: 'bigquery_test1',
          kind: 'bigquery',
          tables: [
            {
              table: { dataset: 'bigquery_sample', name: 'sample_table' },
              object_relationships: [
                {
                  name: 'bq_test_relation',
                  using: {
                    manual_configuration: {
                      column_mapping: { Period: 'STATUS' },
                      insertion_order: null,
                      remote_table: {
                        dataset: 'bigquery_sample',
                        name: 'sample_table',
                      },
                    },
                  },
                },
                {
                  name: 'test_2',
                  using: {
                    manual_configuration: {
                      column_mapping: { Period: 'Series_title_2' },
                      insertion_order: null,
                      remote_table: {
                        dataset: 'bigquery_sample',
                        name: 'sample_table',
                      },
                    },
                  },
                },
              ],
              select_permissions: [
                {
                  role: 'asdf',
                  permission: {
                    columns: [],
                    filter: { _not: { Data_value: { _eq: 1337 } } },
                  },
                },
                {
                  role: 'new',
                  permission: {
                    columns: ['Series_reference', 'Period'],
                    filter: {
                      _and: [
                        { Data_value: { _eq: 'X-Hasura-User-Id' } },
                        { Group: { _eq: 'X-Hasura-User-Id' } },
                      ],
                    },
                    allow_aggregations: true,
                    query_root_fields: [],
                    subscription_root_fields: [],
                  },
                },
                {
                  role: 'sdfsf',
                  permission: {
                    columns: [
                      'Series_reference',
                      'Period',
                      'Data_value',
                      'Suppressed',
                      'STATUS',
                      'UNITS',
                      'Magnitude',
                      'Subject',
                      'Group',
                      'Series_title_1',
                      'Series_title_2',
                      'Series_title_3',
                      'Series_title_4',
                      'Series_title_5',
                    ],
                    filter: {
                      _and: [{ Series_reference: { _eq: 'X-Hasura-User-Id' } }],
                    },
                  },
                },
                {
                  role: 'testrole',
                  permission: {
                    columns: [],
                    filter: { Magnitude: { _eq: '123' } },
                  },
                },
                {
                  role: 'user',
                  permission: {
                    columns: [
                      'Series_reference',
                      'Period',
                      'Data_value',
                      'Suppressed',
                      'STATUS',
                      'UNITS',
                      'Magnitude',
                      'Subject',
                      'Group',
                      'Series_title_1',
                      'Series_title_2',
                      'Series_title_3',
                      'Series_title_4',
                      'Series_title_5',
                    ],
                    filter: {},
                  },
                },
              ],
            },
          ],
          configuration: {
            datasets: ['bigquery_sample'],
            global_select_limit: '1.0',
            project_id: 'sensei-1244',
            service_account: {
              client_email: 'sensei-1244@appspot.gserviceaccount.com',
              private_key:
                '-----BEGIN PRIVATE KEY-----\nMIIEugIBADANBgkqhkiG9w0BAQEFAASCBKQwggSgAgEAAoIBAQDF0IZWOvFfkvv5\nmKIC0FGtjj58zY09mh9Rz9+CEk5ehFU9yaqIcysXLSNlgPCIoZ/aeqkIWMLk7tPI\nKh7bo3iAcXPzmGoFEQvouJklyHITKFuBuKKHc8ZuLziF7LUK3rVsSNyjmHILVXpu\nZKEaX1ROL8NPE09uscDxzmWYbjW2kpYt8J+N/OGx9AYd+M0mxALUQjz31403dwJl\n1pwZZlW9SrpI0drlevUTRdXLCkB3wZIByyd6yUhuvs5RNy6Hwqd010oRWk4BHWIB\nTK+djVLktJ2C1fbco/AobkUGDZEshHiwdUzFV1JcNMAnu3hOJ8TKXUPrORst7DtO\n/UVx+DH3AgMBAAECgf8XTKrb/FTePznA3ekqLCVngDCEMHuuLnc+pvwAmzgzfWpC\nJbitI4u9Ups4A356fz6o8u0qSHFnHucnGratIKxX22oG1TA5aIzN9qlFsbeRKsy0\nuHq+hqqMDJsrorBCFpKJU/kCtnEVzGCOgkJzydAL+wY2w6FBj8h036R61HjiMS6V\nQqhpMQJCj7HqNl0PLmAUYE6odIC1S3MSkh5ReKCKjMN59uep7OZp154HBM+nUPti\n8hI0hjqZWcbucruUbyuf94Xds5OopJbzVtqunDfWosSXi6zi1FijvIpDtA7OoSVl\nRDqzf06q9uOXAc88kVLbtNXJzBxI7jId7ZN2lXECgYEA8p7Rn4vHdum8EZxpYoQ9\nHhGkVxQGyuasuoy2Se19WtWEzIz9HtvQRRdGyvb18tpv/OXQuq3IRWeqi53EZJdO\nmh3xABEyXIznZqw9qR0Nqh+8Y7hkh4KY+XjtzZwzMAP4ryA3tXXcci6uGIXjI9F5\nT3tkHW1OhK7jugrIIApFeA8CgYEA0LkpaSyy0GxPRC9uMQ0EClTeqD+g2qWsf1qo\nhZ6hjujPvC6dwF8TxjSn9e92GY1dCuWv9Jl4hhn0f8NyC7M7658LhJ4yX/zJl+ml\nyKj9AVu+lrvSIeFQgVvx5WcqnyH3lE3Wbz34euEqHXEmncZCtO465tzQvPa48bZr\n7qXcf5kCgYAKqmKqsdld596Fo6uaUhfht4LS9SVrTAKx3LeyvkbxdEt3Nod2ENGy\nB9jr2TlsqJ+drXikjDkum+UKaDAW//PLoJ9Ukmz1VphhkhbP1WAhZ22/sH5y2pHg\n/ajujfxOEw6enDr2tiyvtE+g8lnO3EmJ7fGzvXdjK9EE/65Wgj0VjwKBgA/7PXp6\nG67tvBwCbbN/xaFs4ieLx6s2KEEQFfs1LDWfCdjk0Ntb7E2u1nV0QlyugENHVvOp\nlJrf/eKSenb+4EWoRd9/pWfIGT9yemkk10p1VeCAvRQvBlUBT0I7Zf4NQcGvPA4E\ndrQh7XGXqldGSPFFlGj821a8pWiMiTMarAlxAoGAMfvSYuQOvuYPGQNB/D02CVKh\n8vXfhreW2AskvSwQD/GQowoAJHSxFcwuVKmVt9RTxyl8nFdP39jW/pgfERZG/Wdp\nwdEPaWnmQMuuxHWu74nAhkkqWL4Ic11QgDXbDM8G6/lLwomjn7nad76VaJKZ7XOc\nSxm7dnKiboH1AfjKnbw=\n-----END PRIVATE KEY-----\n',
              project_id: 'sensei-1244',
            },
          },
        },
        {
          name: 'cockroach',
          kind: 'cockroach',
          tables: [
            { table: { name: 'table1', schema: 'public' } },
            { table: { name: 'table2', schema: 'public' } },
          ],
          configuration: {
            connection_info: {
              database_url:
                'postgresql://root@host.docker.internal:26257/defaultdb',
              isolation_level: 'read-committed',
              use_prepared_statements: false,
            },
          },
        },
      ],
      actions: [
        {
          name: 'actionName',
          definition: {
            handler: 'http://host.docker.internal:3000',
            output_type: 'SampleOutput',
            arguments: [{ name: 'arg1', type: 'SampleInput!' }],
            type: 'mutation',
            kind: 'synchronous',
          },
          comment: 'test',
          permissions: [{ role: 'user' }],
        },
      ],
      custom_types: {
        input_objects: [
          {
            name: 'SampleInput',
            fields: [
              { name: 'username', type: 'String!' },
              { name: 'password', type: 'String!' },
            ],
          },
        ],
        objects: [
          {
            name: 'SampleOutput',
            fields: [{ name: 'accessToken', type: 'String!' }],
          },
        ],
      },
      backend_configs: {
        dataconnector: {
          sqlagent: { uri: 'http://host.docker.internal:8100' },
        },
      },
    },
  },
  tableColumns: [
    {
      name: 'Series_reference',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Period',
      dataType: 'FLOAT64',
      consoleDataType: 'number',
      nullable: false,
    },
    {
      name: 'Data_value',
      dataType: 'FLOAT64',
      consoleDataType: 'number',
      nullable: false,
    },
    {
      name: 'Suppressed',
      dataType: 'BOOL',
      consoleDataType: 'boolean',
      nullable: false,
    },
    {
      name: 'STATUS',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'UNITS',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Magnitude',
      dataType: 'INT64',
      consoleDataType: 'number',
      nullable: false,
    },
    {
      name: 'Subject',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Group',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_1',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_2',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_3',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_4',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
    {
      name: 'Series_title_5',
      dataType: 'STRING',
      consoleDataType: 'string',
      nullable: false,
    },
  ],
  trackedTables: [
    {
      table: { dataset: 'bigquery_sample', name: 'sample_table' },
      object_relationships: [
        {
          name: 'bq_test_relation',
          using: {
            manual_configuration: {
              column_mapping: { Period: 'STATUS' },
              insertion_order: null,
              remote_table: {
                dataset: 'bigquery_sample',
                name: 'sample_table',
              },
            },
          },
        },
        {
          name: 'test_2',
          using: {
            manual_configuration: {
              column_mapping: { Period: 'Series_title_2' },
              insertion_order: null,
              remote_table: {
                dataset: 'bigquery_sample',
                name: 'sample_table',
              },
            },
          },
        },
      ],
      select_permissions: [
        {
          role: 'asdf',
          permission: {
            columns: [],
            filter: { _not: { Data_value: { _eq: 1337 } } },
          },
        },
        {
          role: 'new',
          permission: {
            columns: ['Series_reference', 'Period'],
            filter: {
              _and: [
                { Data_value: { _eq: 'X-Hasura-User-Id' } },
                { Group: { _eq: 'X-Hasura-User-Id' } },
              ],
            },
            allow_aggregations: true,
            query_root_fields: [],
            subscription_root_fields: [],
          },
        },
        {
          role: 'sdfsf',
          permission: {
            columns: [
              'Series_reference',
              'Period',
              'Data_value',
              'Suppressed',
              'STATUS',
              'UNITS',
              'Magnitude',
              'Subject',
              'Group',
              'Series_title_1',
              'Series_title_2',
              'Series_title_3',
              'Series_title_4',
              'Series_title_5',
            ],
            filter: {
              _and: [{ Series_reference: { _eq: 'X-Hasura-User-Id' } }],
            },
          },
        },
        {
          role: 'testrole',
          permission: { columns: [], filter: { Magnitude: { _eq: '123' } } },
        },
        {
          role: 'user',
          permission: {
            columns: [
              'Series_reference',
              'Period',
              'Data_value',
              'Suppressed',
              'STATUS',
              'UNITS',
              'Magnitude',
              'Subject',
              'Group',
              'Series_title_1',
              'Series_title_2',
              'Series_title_3',
              'Series_title_4',
              'Series_title_5',
            ],
            filter: {},
          },
        },
      ],
    },
  ],
  metadataSource: {
    name: 'bigquery_test1',
    kind: 'bigquery',
    tables: [
      {
        table: { dataset: 'bigquery_sample', name: 'sample_table' },
        object_relationships: [
          {
            name: 'bq_test_relation',
            using: {
              manual_configuration: {
                column_mapping: { Period: 'STATUS' },
                insertion_order: null,
                remote_table: {
                  dataset: 'bigquery_sample',
                  name: 'sample_table',
                },
              },
            },
          },
          {
            name: 'test_2',
            using: {
              manual_configuration: {
                column_mapping: { Period: 'Series_title_2' },
                insertion_order: null,
                remote_table: {
                  dataset: 'bigquery_sample',
                  name: 'sample_table',
                },
              },
            },
          },
        ],
        select_permissions: [
          {
            role: 'asdf',
            permission: {
              columns: [],
              filter: { _not: { Data_value: { _eq: 1337 } } },
            },
          },
          {
            role: 'new',
            permission: {
              columns: ['Series_reference', 'Period'],
              filter: {
                _and: [
                  { Data_value: { _eq: 'X-Hasura-User-Id' } },
                  { Group: { _eq: 'X-Hasura-User-Id' } },
                ],
              },
              allow_aggregations: true,
              query_root_fields: [],
              subscription_root_fields: [],
            },
          },
          {
            role: 'sdfsf',
            permission: {
              columns: [
                'Series_reference',
                'Period',
                'Data_value',
                'Suppressed',
                'STATUS',
                'UNITS',
                'Magnitude',
                'Subject',
                'Group',
                'Series_title_1',
                'Series_title_2',
                'Series_title_3',
                'Series_title_4',
                'Series_title_5',
              ],
              filter: {
                _and: [{ Series_reference: { _eq: 'X-Hasura-User-Id' } }],
              },
            },
          },
          {
            role: 'testrole',
            permission: { columns: [], filter: { Magnitude: { _eq: '123' } } },
          },
          {
            role: 'user',
            permission: {
              columns: [
                'Series_reference',
                'Period',
                'Data_value',
                'Suppressed',
                'STATUS',
                'UNITS',
                'Magnitude',
                'Subject',
                'Group',
                'Series_title_1',
                'Series_title_2',
                'Series_title_3',
                'Series_title_4',
                'Series_title_5',
              ],
              filter: {},
            },
          },
        ],
      },
    ],
  },
} as any;
