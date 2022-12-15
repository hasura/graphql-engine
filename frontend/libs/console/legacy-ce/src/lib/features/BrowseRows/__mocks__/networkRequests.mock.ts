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
        queryType: { name: 'query_root' },
        mutationType: null,
        subscriptionType: { name: 'subscription_root' },
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
                    name: 'decimal',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Artist',
                description: 'An object relationship',
                args: [],
                type: {
                  kind: 'OBJECT',
                  name: 'Artist',
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
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'SCALAR',
                    name: 'decimal',
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
                    name: 'String',
                    ofType: null,
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Tracks',
                description: 'An array relationship',
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
                          name: 'Track_order_by',
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
                      name: 'Track_bool_exp',
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
                        name: 'Track',
                        ofType: null,
                      },
                    },
                  },
                },
                isDeprecated: false,
                deprecationReason: null,
              },
              {
                name: 'Tracks_aggregate',
                description: 'An aggregate relationship',
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
                          name: 'Track_order_by',
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
                      name: 'Track_bool_exp',
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
                    name: 'Track_aggregate',
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
                    name: 'decimal',
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
                  name: 'String',
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
            name: 'decimal',
            description: null,
            fields: null,
            inputFields: null,
            interfaces: null,
            enumValues: null,
            possibleTypes: null,
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
