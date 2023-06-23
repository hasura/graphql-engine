export const metadataRequestResponseMap: Record<string, any> = {
  [JSON.stringify({ type: 'export_metadata', version: 2, args: {} })]: {
    resource_version: 202,
    metadata: {
      version: 3,
      sources: [
        {
          name: 'sqlite_test',
          kind: 'sqlite',
          tables: [
            {
              table: ['Album'],
              object_relationships: [
                {
                  name: 'Artist',
                  using: {
                    manual_configuration: {
                      column_mapping: { ArtistId: 'ArtistId' },
                      insertion_order: null,
                      remote_table: ['Artist'],
                    },
                  },
                },
                {
                  name: 'Tracks',
                  using: {
                    manual_configuration: {
                      column_mapping: { AlbumId: 'AlbumId' },
                      insertion_order: null,
                      remote_table: ['Track'],
                    },
                  },
                },
              ],
            },
            { table: ['Artist'] },
            { table: ['Genre'] },
            { table: ['Track'] },
          ],
          configuration: {
            template: null,
            timeout: null,
            value: {
              db: './chinook.db',
              explicit_main_schema: false,
              include_sqlite_meta_tables: false,
              tables: ['Album', 'Artist', 'Genre', 'Track'],
            },
          },
        },
      ],
      backend_configs: {
        dataconnector: { sqlite: { uri: 'http://host.docker.internal:8100' } },
      },
    },
  },
  [JSON.stringify({
    type: 'get_table_info',
    args: { source: 'sqlite_test', table: ['Album'] },
  })]: {
    name: ['Album'],
    columns: [
      { name: 'AlbumId', type: 'number', nullable: false },
      { name: 'Title', type: 'string', nullable: false },
      { name: 'ArtistId', type: 'number', nullable: false },
    ],
    primary_key: ['AlbumId'],
    foreign_keys: {
      'ArtistId->Artist.ArtistId': {
        foreign_table: ['Artist'],
        column_mapping: { ArtistId: 'ArtistId' },
      },
    },
    description:
      'CREATE TABLE [Album]\n(\n    [AlbumId] INTEGER  NOT NULL,\n    [Title] NVARCHAR(160)  NOT NULL,\n    [ArtistId] INTEGER  NOT NULL,\n    CONSTRAINT [PK_Album] PRIMARY KEY  ([AlbumId]),\n    FOREIGN KEY ([ArtistId]) REFERENCES [Artist] ([ArtistId]) \n\t\tON DELETE NO ACTION ON UPDATE NO ACTION\n)',
  },
  [JSON.stringify({
    type: 'get_table_info',
    args: { source: 'sqlite_test', table: ['Artist'] },
  })]: {
    name: ['Artist'],
    columns: [
      { name: 'ArtistId', type: 'number', nullable: false },
      { name: 'Name', type: 'string', nullable: true },
    ],
    primary_key: ['ArtistId'],
    description:
      'CREATE TABLE [Artist]\n(\n    [ArtistId] INTEGER  NOT NULL,\n    [Name] NVARCHAR(120),\n    CONSTRAINT [PK_Artist] PRIMARY KEY  ([ArtistId])\n)',
  },
};

export const graphqlRequestResponseMap = {
  [JSON.stringify({
    query:
      'query IntrospectionQuery {\n      __schema {\n        queryType {\n          name\n        }\n        mutationType {\n          name\n        }\n        subscriptionType {\n          name\n        }\n        types {\n          ...FullType\n        }\n        directives {\n          name\n          description\n          locations\n          args {\n            ...InputValue\n          }\n        }\n      }\n    }\n    fragment FullType on __Type {\n      kind\n      name\n      description\n      fields(includeDeprecated: true) {\n        name\n        description\n        args {\n          ...InputValue\n        }\n        type {\n          ...TypeRef\n        }\n        isDeprecated\n        deprecationReason\n      }\n      inputFields {\n        ...InputValue\n      }\n      interfaces {\n        ...TypeRef\n      }\n      enumValues(includeDeprecated: true) {\n        name\n        description\n        isDeprecated\n        deprecationReason\n      }\n      possibleTypes {\n        ...TypeRef\n      }\n    }\n    fragment InputValue on __InputValue {\n      name\n      description\n      type {\n        ...TypeRef\n      }\n      defaultValue\n    }\n    fragment TypeRef on __Type {\n      kind\n      name\n      ofType {\n        kind\n        name\n        ofType {\n          kind\n          name\n          ofType {\n            kind\n            name\n            ofType {\n              kind\n              name\n              ofType {\n                kind\n                name\n                ofType {\n                  kind\n                  name\n                  ofType {\n                    kind\n                    name\n                  }\n                }\n              }\n            }\n          }\n        }\n      }\n    }',
    operationName: 'IntrospectionQuery',
  })]: {
    data: {
      __schema: {
        queryType: {
          name: 'query_root',
        },
        mutationType: null,
        subscriptionType: {
          name: 'subscription_root',
        },
        types: [
          {
            kind: 'OBJECT',
            name: 'Album',
            description:
              'CREATE TABLE [Album]\n(\n    [AlbumId] INTEGER  NOT NULL,\n    [Title] NVARCHAR(160)  NOT NULL,\n    [ArtistId] INTEGER  NOT NULL,\n    CONSTRAINT [PK_Album] PRIMARY KEY  ([AlbumId]),\n    FOREIGN KEY ([ArtistId]) REFERENCES [Artist] ([ArtistId]) \n\t\tON DELETE NO ACTION ON UPDATE NO ACTION\n)',
            fields: [
              {
                name: 'AlbumId',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'number',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'ArtistId',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'number',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Title',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'string',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Album_aggregate',
            description: 'aggregated selection of "Album"',
            fields: [
              {
                name: 'aggregate',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'Album_aggregate_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'nodes',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'Album',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Album_aggregate_fields',
            description: 'aggregate fields of "Album"',
            fields: [
              {
                name: 'count',
                description: null,
                args: [
                  {
                    name: 'column',
                    description: null,
                    type: {
                      kind: 'ENUM',
                      name: 'Album_select_column',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'distinct',
                    description: null,
                    type: {
                      kind: 'SCALAR',
                      name: 'Boolean',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'max',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'Album_max_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'min',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'Album_min_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'sum',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'Album_sum_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'Album_bool_exp',
            description:
              'Boolean expression to filter rows from the table "Album". All fields are combined with a logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: 'AlbumId',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'number_SQLite_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'ArtistId',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'number_SQLite_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'Title',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'string_SQLite_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_and',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Album_bool_exp',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_not',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'Album_bool_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_or',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Album_bool_exp',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Album_max_fields',
            description: 'aggregate max on columns',
            fields: [
              {
                name: 'AlbumId',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'ArtistId',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Title',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Album_min_fields',
            description: 'aggregate min on columns',
            fields: [
              {
                name: 'AlbumId',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'ArtistId',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Title',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'Album_order_by',
            description: 'Ordering options when selecting data from "Album".',
            fields: null,
            inputFields: [
              {
                name: 'AlbumId',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'SQLite_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'ArtistId',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'SQLite_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'Title',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'SQLite_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'ENUM',
            name: 'Album_select_column',
            description: 'select columns of table "Album"',
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: [
              {
                name: 'AlbumId',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'ArtistId',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Title',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Album_sum_fields',
            description: 'aggregate sum on columns',
            fields: [
              {
                name: 'AlbumId',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'ArtistId',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Artist',
            description:
              'CREATE TABLE [Artist]\n(\n    [ArtistId] INTEGER  NOT NULL,\n    [Name] NVARCHAR(120),\n    CONSTRAINT [PK_Artist] PRIMARY KEY  ([ArtistId])\n)',
            fields: [
              {
                name: 'ArtistId',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'number',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Name',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Artist_aggregate',
            description: 'aggregated selection of "Artist"',
            fields: [
              {
                name: 'aggregate',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'Artist_aggregate_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'nodes',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'Artist',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Artist_aggregate_fields',
            description: 'aggregate fields of "Artist"',
            fields: [
              {
                name: 'count',
                description: null,
                args: [
                  {
                    name: 'column',
                    description: null,
                    type: {
                      kind: 'ENUM',
                      name: 'Artist_select_column',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'distinct',
                    description: null,
                    type: {
                      kind: 'SCALAR',
                      name: 'Boolean',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'max',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'Artist_max_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'min',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'Artist_min_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'sum',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'Artist_sum_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'Artist_bool_exp',
            description:
              'Boolean expression to filter rows from the table "Artist". All fields are combined with a logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: 'ArtistId',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'number_SQLite_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'Name',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'string_SQLite_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_and',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Artist_bool_exp',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_not',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'Artist_bool_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_or',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Artist_bool_exp',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Artist_max_fields',
            description: 'aggregate max on columns',
            fields: [
              {
                name: 'ArtistId',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Name',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Artist_min_fields',
            description: 'aggregate min on columns',
            fields: [
              {
                name: 'ArtistId',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Name',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'Artist_order_by',
            description: 'Ordering options when selecting data from "Artist".',
            fields: null,
            inputFields: [
              {
                name: 'ArtistId',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'SQLite_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'Name',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'SQLite_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'ENUM',
            name: 'Artist_select_column',
            description: 'select columns of table "Artist"',
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: [
              {
                name: 'ArtistId',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Name',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'Artist_sum_fields',
            description: 'aggregate sum on columns',
            fields: [
              {
                name: 'ArtistId',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'SCALAR',
            name: 'Boolean',
            description: null,
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'SCALAR',
            name: 'Int',
            description: null,
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'ENUM',
            name: 'SQLite_order_by',
            description: 'column ordering options',
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: [
              {
                name: 'asc',
                description: 'in ascending order',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'desc',
                description: 'in descending order',
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            possibleTypes: null,
          },
          {
            kind: 'SCALAR',
            name: 'String',
            description: null,
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: '__Directive',
            description: null,
            fields: [
              {
                name: 'args',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__InputValue',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'description',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'isRepeatable',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'locations',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'name',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: '__EnumValue',
            description: null,
            fields: [
              {
                name: 'deprecationReason',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'description',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'isDeprecated',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'name',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: '__Field',
            description: null,
            fields: [
              {
                name: 'args',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__InputValue',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'deprecationReason',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'description',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'isDeprecated',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'name',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'type',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: '__InputValue',
            description: null,
            fields: [
              {
                name: 'defaultValue',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'description',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'name',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'type',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: '__Schema',
            description: null,
            fields: [
              {
                name: 'description',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'directives',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Directive',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'mutationType',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'queryType',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'subscriptionType',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'types',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: '__Type',
            description: null,
            fields: [
              {
                name: 'description',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'enumValues',
                description: null,
                args: [
                  {
                    name: 'includeDeprecated',
                    description: null,
                    type: {
                      kind: 'SCALAR',
                      name: 'Boolean',
                      ofType: null,
                    },
                    defaultValue: 'false',
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: '__EnumValue',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'fields',
                description: null,
                args: [
                  {
                    name: 'includeDeprecated',
                    description: null,
                    type: {
                      kind: 'SCALAR',
                      name: 'Boolean',
                      ofType: null,
                    },
                    defaultValue: 'false',
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: '__Field',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'inputFields',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__InputValue',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'interfaces',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'kind',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'ENUM',
                    name: '__TypeKind',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'name',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'ofType',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'possibleTypes',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: '__Type',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'ENUM',
            name: '__TypeKind',
            description: null,
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: [
              {
                name: 'ENUM',
                description: null,
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'INPUT_OBJECT',
                description: null,
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'INTERFACE',
                description: null,
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'LIST',
                description: null,
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'NON_NULL',
                description: null,
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'OBJECT',
                description: null,
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'SCALAR',
                description: null,
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'UNION',
                description: null,
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            possibleTypes: null,
          },
          {
            kind: 'SCALAR',
            name: 'double',
            description: 'A custom scalar type',
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'double_mongodb_comparison_exp',
            description:
              'Boolean expression to compare columns of type "double". All fields are combined with logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: '_eq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'double',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'double',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'double',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_in',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'double',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_is_null',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'double',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'double',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_neq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'double',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_nin',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'double',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'SCALAR',
            name: 'int',
            description: 'A custom scalar type',
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'int_mongodb_comparison_exp',
            description:
              'Boolean expression to compare columns of type "int". All fields are combined with logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: '_eq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_in',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'int',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_is_null',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_neq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'int',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_nin',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'int',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'ENUM',
            name: 'mongodb_order_by',
            description: 'column ordering options',
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: [
              {
                name: 'asc',
                description: 'in ascending order',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'desc',
                description: 'in descending order',
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'mycollection',
            description: 'columns and relationships of "mycollection"',
            fields: [
              {
                name: '_id',
                description: 'primary key _id',
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'objectId',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'age',
                description: 'must be an integer and is required',
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'int',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'email',
                description:
                  'must be a string and match the regular expression pattern',
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'name',
                description: 'must be a string and is required',
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'string',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'mycollection_aggregate',
            description: 'aggregated selection of "mycollection"',
            fields: [
              {
                name: 'aggregate',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'mycollection_aggregate_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'nodes',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'mycollection',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'mycollection_aggregate_fields',
            description: 'aggregate fields of "mycollection"',
            fields: [
              {
                name: 'count',
                description: null,
                args: [
                  {
                    name: 'column',
                    description: null,
                    type: {
                      kind: 'ENUM',
                      name: 'mycollection_select_column',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'distinct',
                    description: null,
                    type: {
                      kind: 'SCALAR',
                      name: 'Boolean',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'mycollection_bool_exp',
            description:
              'Boolean expression to filter rows from the table "mycollection". All fields are combined with a logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: '_and',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'mycollection_bool_exp',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_id',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'objectId_mongodb_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_not',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'mycollection_bool_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_or',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'mycollection_bool_exp',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'age',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'int_mongodb_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'email',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'string_mongodb_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'name',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'string_mongodb_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'mycollection_order_by',
            description:
              'Ordering options when selecting data from "mycollection".',
            fields: null,
            inputFields: [
              {
                name: '_id',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'mongodb_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'age',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'mongodb_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'email',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'mongodb_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'name',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'mongodb_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'ENUM',
            name: 'mycollection_select_column',
            description: 'select columns of table "mycollection"',
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: [
              {
                name: '_id',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'age',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'email',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'name',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            possibleTypes: null,
          },
          {
            kind: 'SCALAR',
            name: 'number',
            description: null,
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'number_SQLite_comparison_exp',
            description:
              'Boolean expression to compare columns of type "number". All fields are combined with logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: '_eq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_in',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'number',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_is_null',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_modulus_is_zero',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_neq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'number',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_nin',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'number',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'SCALAR',
            name: 'objectId',
            description: 'A custom scalar type',
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'objectId_mongodb_comparison_exp',
            description:
              'Boolean expression to compare columns of type "objectId". All fields are combined with logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: '_eq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'objectId',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'objectId',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'objectId',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_in',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'objectId',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_is_null',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'objectId',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'objectId',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_neq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'objectId',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_nin',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'objectId',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'query_root',
            description: null,
            fields: [
              {
                name: 'Album',
                description: 'fetch data from the table: "Album"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'Album_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'Album_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'Album',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Album_aggregate',
                description: 'fetch aggregated fields from the table: "Album"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'Album_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'Album_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'Album_aggregate',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Album_by_pk',
                description:
                  'fetch data from the table: "Album" using primary key columns',
                args: [
                  {
                    name: 'AlbumId',
                    description: null,
                    type: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'SCALAR',
                        name: 'number',
                        ofType: null,
                      },
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: 'Album',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Artist',
                description: 'fetch data from the table: "Artist"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'Artist_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'Artist_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'Artist',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Artist_aggregate',
                description: 'fetch aggregated fields from the table: "Artist"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'Artist_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'Artist_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'Artist_aggregate',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Artist_by_pk',
                description:
                  'fetch data from the table: "Artist" using primary key columns',
                args: [
                  {
                    name: 'ArtistId',
                    description: null,
                    type: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'SCALAR',
                        name: 'number',
                        ofType: null,
                      },
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: 'Artist',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'mycollection',
                description: 'fetch data from the table: "mycollection"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'mycollection_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'mycollection_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'mycollection',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'mycollection_aggregate',
                description:
                  'fetch aggregated fields from the table: "mycollection"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'mycollection_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'mycollection_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'mycollection_aggregate',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'mycollection_by_pk',
                description:
                  'fetch data from the table: "mycollection" using primary key columns',
                args: [
                  {
                    name: '_id',
                    description: 'primary key _id',
                    type: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'SCALAR',
                        name: 'objectId',
                        ofType: null,
                      },
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: 'mycollection',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'students',
                description: 'fetch data from the table: "students"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'students_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'students_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'students',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'students_aggregate',
                description:
                  'fetch aggregated fields from the table: "students"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'students_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'students_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'students_aggregate',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'students_by_pk',
                description:
                  'fetch data from the table: "students" using primary key columns',
                args: [
                  {
                    name: '_id',
                    description: 'primary key _id',
                    type: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'SCALAR',
                        name: 'objectId',
                        ofType: null,
                      },
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: 'students',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'SCALAR',
            name: 'string',
            description: null,
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'string_SQLite_comparison_exp',
            description:
              'Boolean expression to compare columns of type "string". All fields are combined with logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: '_eq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_glob',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_in',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'string',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_is_null',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_like',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_neq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_nin',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'string',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'string_mongodb_comparison_exp',
            description:
              'Boolean expression to compare columns of type "string". All fields are combined with logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: '_eq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_gte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_in',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'string',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_is_null',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'Boolean',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lt',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_lte',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_neq',
                description: null,
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_nin',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'string',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'students',
            description: 'columns and relationships of "students"',
            fields: [
              {
                name: '_id',
                description: 'primary key _id',
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'objectId',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'address',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'students_address',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'gpa',
                description: "'gpa' must be a double if the field exists",
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'double',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'name',
                description: "'name' must be a string and is required",
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'string',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'year',
                description:
                  "'year' must be an integer in [ 2017, 3017 ] and is required",
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'int',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'students_address',
            description: 'generated from MongoDB validation schema',
            fields: [
              {
                name: 'city',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'street',
                description: null,
                args: [],
                type: {
                  kind: 'SCALAR',
                  name: 'string',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'students_aggregate',
            description: 'aggregated selection of "students"',
            fields: [
              {
                name: 'aggregate',
                description: null,
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'students_aggregate_fields',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'nodes',
                description: null,
                args: [],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'students',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'students_aggregate_fields',
            description: 'aggregate fields of "students"',
            fields: [
              {
                name: 'count',
                description: null,
                args: [
                  {
                    name: 'column',
                    description: null,
                    type: {
                      kind: 'ENUM',
                      name: 'students_select_column',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'distinct',
                    description: null,
                    type: {
                      kind: 'SCALAR',
                      name: 'Boolean',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'students_bool_exp',
            description:
              'Boolean expression to filter rows from the table "students". All fields are combined with a logical \'AND\'.',
            fields: null,
            inputFields: [
              {
                name: '_and',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'students_bool_exp',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: '_id',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'objectId_mongodb_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_not',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'students_bool_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: '_or',
                description: null,
                type: {
                  kind: 'LIST',
                  name: null,
                  ofType: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'students_bool_exp',
                      ofType: null,
                    },
                  },
                },
                defaultValue: null,
              },
              {
                name: 'gpa',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'double_mongodb_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'name',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'string_mongodb_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'year',
                description: null,
                type: {
                  kind: 'INPUT_OBJECT',
                  name: 'int_mongodb_comparison_exp',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'INPUT_OBJECT',
            name: 'students_order_by',
            description:
              'Ordering options when selecting data from "students".',
            fields: null,
            inputFields: [
              {
                name: '_id',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'mongodb_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'gpa',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'mongodb_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'name',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'mongodb_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
              {
                name: 'year',
                description: null,
                type: {
                  kind: 'ENUM',
                  name: 'mongodb_order_by',
                  ofType: null,
                },
                defaultValue: null,
              },
            ],
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
          },
          {
            kind: 'ENUM',
            name: 'students_select_column',
            description: 'select columns of table "students"',
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: [
              {
                name: '_id',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'address',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'gpa',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'name',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'year',
                description: 'column name',
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            possibleTypes: null,
          },
          {
            kind: 'OBJECT',
            name: 'subscription_root',
            description: null,
            fields: [
              {
                name: 'Album',
                description: 'fetch data from the table: "Album"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'Album_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'Album_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'Album',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Album_aggregate',
                description: 'fetch aggregated fields from the table: "Album"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'Album_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'Album_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'Album_aggregate',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Album_by_pk',
                description:
                  'fetch data from the table: "Album" using primary key columns',
                args: [
                  {
                    name: 'AlbumId',
                    description: null,
                    type: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'SCALAR',
                        name: 'number',
                        ofType: null,
                      },
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: 'Album',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Artist',
                description: 'fetch data from the table: "Artist"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'Artist_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'Artist_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'Artist',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Artist_aggregate',
                description: 'fetch aggregated fields from the table: "Artist"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'Artist_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'Artist_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'Artist_aggregate',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Artist_by_pk',
                description:
                  'fetch data from the table: "Artist" using primary key columns',
                args: [
                  {
                    name: 'ArtistId',
                    description: null,
                    type: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'SCALAR',
                        name: 'number',
                        ofType: null,
                      },
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: 'Artist',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'mycollection',
                description: 'fetch data from the table: "mycollection"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'mycollection_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'mycollection_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'mycollection',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'mycollection_aggregate',
                description:
                  'fetch aggregated fields from the table: "mycollection"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'mycollection_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'mycollection_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'mycollection_aggregate',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'mycollection_by_pk',
                description:
                  'fetch data from the table: "mycollection" using primary key columns',
                args: [
                  {
                    name: '_id',
                    description: 'primary key _id',
                    type: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'SCALAR',
                        name: 'objectId',
                        ofType: null,
                      },
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: 'mycollection',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'students',
                description: 'fetch data from the table: "students"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'students_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'students_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'OBJECT',
                        name: 'students',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'students_aggregate',
                description:
                  'fetch aggregated fields from the table: "students"',
                args: [
                  {
                    name: 'limit',
                    description: 'limit the number of rows returned',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'offset',
                    description:
                      'skip the first n rows. Use only with order_by',
                    type: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'order_by',
                    description: 'sort the rows by one or more columns',
                    type: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'NON_NULL',
                        name: null,
                        ofType: {
                          kind: 'INPUT_OBJECT',
                          name: 'students_order_by',
                          ofType: null,
                        },
                      },
                    },
                    defaultValue: null,
                  },
                  {
                    name: 'where',
                    description: 'filter the rows returned',
                    type: {
                      kind: 'INPUT_OBJECT',
                      name: 'students_bool_exp',
                      ofType: null,
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'OBJECT',
                    name: 'students_aggregate',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'students_by_pk',
                description:
                  'fetch data from the table: "students" using primary key columns',
                args: [
                  {
                    name: '_id',
                    description: 'primary key _id',
                    type: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'SCALAR',
                        name: 'objectId',
                        ofType: null,
                      },
                    },
                    defaultValue: null,
                  },
                ],
                type: {
                  kind: 'OBJECT',
                  name: 'students',
                  ofType: null,
                },
                isDeprecated: false,
                deprecationReason: null,
              },
            ],
            inputFields: null,
            interfaces: [],
            enumValues: null,
            possibleTypes: null,
          },
        ],
        directives: [
          {
            name: 'include',
            description: 'whether this query should be included',
            locations: ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
            args: [
              {
                name: 'if',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Boolean',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
          },
          {
            name: 'skip',
            description: 'whether this query should be skipped',
            locations: ['FIELD', 'FRAGMENT_SPREAD', 'INLINE_FRAGMENT'],
            args: [
              {
                name: 'if',
                description: null,
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Boolean',
                    ofType: null,
                  },
                },
                defaultValue: null,
              },
            ],
          },
          {
            name: 'cached',
            description:
              'whether this query should be cached (Hasura Cloud only)',
            locations: ['QUERY'],
            args: [
              {
                name: 'ttl',
                description: 'measured in seconds',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                },
                defaultValue: '60',
              },
              {
                name: 'refresh',
                description: 'refresh the cache entry',
                type: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'Boolean',
                    ofType: null,
                  },
                },
                defaultValue: 'false',
              },
            ],
          },
        ],
      },
    },
  },
  [JSON.stringify({
    query:
      'query TableRows {\n      Album (where: {},limit: 10) {\n        AlbumId \n,Title \n,ArtistId \n\n      }\n    }',
    operationName: 'TableRows',
  })]: {
    data: {
      Album: [
        {
          AlbumId: 1,
          Title: 'For Those About To Rock We Salute You',
          ArtistId: 1,
        },
        { AlbumId: 2, Title: 'Balls to the Wall', ArtistId: 2 },
        { AlbumId: 3, Title: 'Restless and Wild', ArtistId: 2 },
        { AlbumId: 4, Title: 'Let There Be Rock', ArtistId: 1 },
        { AlbumId: 5, Title: 'Big Ones', ArtistId: 3 },
        { AlbumId: 6, Title: 'Jagged Little Pill', ArtistId: 4 },
        { AlbumId: 7, Title: 'Facelift', ArtistId: 5 },
        { AlbumId: 8, Title: 'Warner 25 Anos', ArtistId: 6 },
        { AlbumId: 9, Title: 'Plays Metallica By Four Cellos', ArtistId: 7 },
        { AlbumId: 10, Title: 'Audioslave', ArtistId: 8 },
      ],
    },
  },
  [JSON.stringify({
    query:
      'query TableRows {\n      Album (where: {},limit: 10,offset: 10) {\n        AlbumId \n,Title \n,ArtistId \n\n      }\n    }',
    operationName: 'TableRows',
  })]: {
    data: {
      Album: [
        { AlbumId: 11, Title: 'Out Of Exile', ArtistId: 8 },
        { AlbumId: 12, Title: 'BackBeat Soundtrack', ArtistId: 9 },
        { AlbumId: 13, Title: 'The Best Of Billy Cobham', ArtistId: 10 },
        {
          AlbumId: 14,
          Title: 'Alcohol Fueled Brewtality Live! [Disc 1]',
          ArtistId: 11,
        },
        {
          AlbumId: 15,
          Title: 'Alcohol Fueled Brewtality Live! [Disc 2]',
          ArtistId: 11,
        },
        { AlbumId: 16, Title: 'Black Sabbath', ArtistId: 12 },
        { AlbumId: 17, Title: 'Black Sabbath Vol. 4 (Remaster)', ArtistId: 12 },
        { AlbumId: 18, Title: 'Body Count', ArtistId: 13 },
        { AlbumId: 19, Title: 'Chemical Wedding', ArtistId: 14 },
        {
          AlbumId: 20,
          Title: 'The Best Of Buddy Guy - The Millenium Collection',
          ArtistId: 15,
        },
      ],
    },
  },
  [JSON.stringify({
    query:
      'query TableRows {\n      Artist (where: {ArtistId: { _eq: 1}},limit: 10) {\n        ArtistId \n,Name \n\n      }\n    }',
    operationName: 'TableRows',
  })]: { data: { Artist: [{ ArtistId: 1, Name: 'AC/DC' }] } },
  [JSON.stringify({
    query:
      'query TableRows {\n      Album (where: {AlbumId: { _eq: 1}},limit: 10) {\n        AlbumId \n,Title \n,ArtistId \n\n      }\n    }',
    operationName: 'TableRows',
  })]: {
    data: {
      Album: [
        {
          AlbumId: 1,
          Title: 'For Those About To Rock We Salute You',
          ArtistId: 1,
        },
      ],
    },
  },
};
