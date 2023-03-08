export const introspection = {
  data: {
    __schema: {
      queryType: {
        name: 'query_root',
      },
      mutationType: {
        name: 'mutation_root',
      },
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
              name: 'Creator',
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
              name: 'Creator',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Artist_bool_exp',
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
              name: 'Creator',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Artist_order_by',
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
          kind: 'INPUT_OBJECT',
          name: 'Boolean_BigQuery_comparison_exp',
          description:
            'Boolean expression to compare columns of type "Boolean". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
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
                    name: 'Boolean',
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
                name: 'Boolean',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
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
                    name: 'Boolean',
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
          name: 'City',
          description: 'columns and relationships of "City"',
          fields: [
            {
              name: 'city_id',
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
            {
              name: 'id',
              description: null,
              args: [],
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
          name: 'City_aggregate',
          description: 'aggregated selection of "City"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'City_aggregate_fields',
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
                      name: 'City',
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
          name: 'City_aggregate_fields',
          description: 'aggregate fields of "City"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'City_avg_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'count',
              description: null,
              args: [
                {
                  name: 'columns',
                  description: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'City_select_column',
                        ofType: null,
                      },
                    },
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
                name: 'City_max_fields',
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
                name: 'City_min_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'stddev',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'City_stddev_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'stddev_pop',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'City_stddev_pop_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'stddev_samp',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'City_stddev_samp_fields',
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
                name: 'City_sum_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'var_pop',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'City_var_pop_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'var_samp',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'City_var_samp_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'variance',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'City_variance_fields',
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
          name: 'City_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'City_bool_exp',
          description:
            'Boolean expression to filter rows from the table "City". All fields are combined with a logical \'AND\'.',
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
                    name: 'City_bool_exp',
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
                name: 'City_bool_exp',
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
                    name: 'City_bool_exp',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
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
          name: 'City_constraint',
          description: 'unique or primary key constraints on table "City"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'City_pkey',
              description: 'unique or primary key constraint on columns "id"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'City_inc_input',
          description:
            'input type for incrementing numeric columns in table "City"',
          fields: null,
          inputFields: [
            {
              name: 'id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
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
          name: 'City_insert_input',
          description: 'input type for inserting data into table "City"',
          fields: null,
          inputFields: [
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
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
          kind: 'OBJECT',
          name: 'City_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'city_id',
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
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'name',
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
          kind: 'OBJECT',
          name: 'City_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'city_id',
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
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'name',
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
          kind: 'OBJECT',
          name: 'City_mutation_response',
          description: 'response of any mutation on the table "City"',
          fields: [
            {
              name: 'affected_rows',
              description: 'number of rows affected by the mutation',
              args: [],
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
              name: 'returning',
              description: 'data from the rows affected by the mutation',
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
                      name: 'City',
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
          kind: 'INPUT_OBJECT',
          name: 'City_obj_rel_insert_input',
          description:
            'input type for inserting object relation for remote table "City"',
          fields: null,
          inputFields: [
            {
              name: 'data',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'City_insert_input',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
            {
              name: 'on_conflict',
              description: 'upsert condition',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'City_on_conflict',
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
          name: 'City_on_conflict',
          description: 'on_conflict condition type for table "City"',
          fields: null,
          inputFields: [
            {
              name: 'constraint',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'ENUM',
                  name: 'City_constraint',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
            {
              name: 'update_columns',
              description: null,
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
                      kind: 'ENUM',
                      name: 'City_update_column',
                      ofType: null,
                    },
                  },
                },
              },
              defaultValue: '[]',
            },
            {
              name: 'where',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'City_bool_exp',
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
          name: 'City_order_by',
          description: 'Ordering options when selecting data from "City".',
          fields: null,
          inputFields: [
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
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
          name: 'City_pk_columns_input',
          description: 'primary key columns input for table: City',
          fields: null,
          inputFields: [
            {
              name: 'id',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
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
          name: 'City_select_column',
          description: 'select columns of table "City"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'city_id',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
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
          kind: 'INPUT_OBJECT',
          name: 'City_set_input',
          description: 'input type for updating data in table "City"',
          fields: null,
          inputFields: [
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
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
          kind: 'OBJECT',
          name: 'City_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'City_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'City_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'City_stream_cursor_input',
          description: 'Streaming cursor of the table "City"',
          fields: null,
          inputFields: [
            {
              name: 'initial_value',
              description: 'Stream column input with initial value',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'City_stream_cursor_value_input',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
            {
              name: 'ordering',
              description: 'cursor ordering',
              type: {
                kind: 'ENUM',
                name: 'cursor_ordering',
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
          name: 'City_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
          fields: null,
          inputFields: [
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
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
          kind: 'OBJECT',
          name: 'City_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
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
          name: 'City_update_column',
          description: 'update columns of table "City"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'city_id',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
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
          kind: 'INPUT_OBJECT',
          name: 'City_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'City_inc_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'City_set_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'where',
              description: 'filter the rows which have to be updated',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'City_bool_exp',
                  ofType: null,
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
          name: 'City_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'City_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'City_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Customer',
          description:
            'CREATE TABLE [Customer]\n(\n    [CustomerId] INTEGER  NOT NULL,\n    [FirstName] NVARCHAR(40)  NOT NULL,\n    [LastName] NVARCHAR(20)  NOT NULL,\n    [Company] NVARCHAR(80),\n    [Address] NVARCHAR(70),\n    [City] NVARCHAR(40),\n    [State] NVARCHAR(40),\n    [Country] NVARCHAR(40),\n    [PostalCode] NVARCHAR(10),\n    [Phone] NVARCHAR(24),\n    [Fax] NVARCHAR(24),\n    [Email] NVARCHAR(60)  NOT NULL,\n    [SupportRepId] INTEGER,\n    CONSTRAINT [PK_Customer] PRIMARY KEY  ([CustomerId]),\n    FOREIGN KEY ([SupportRepId]) REFERENCES [Employee] ([EmployeeId]) \n\t\tON DELETE NO ACTION ON UPDATE NO ACTION\n)',
          fields: [
            {
              name: 'Address',
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
              name: 'City',
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
              name: 'Company',
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
              name: 'Country',
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
              name: 'CustomerId',
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
              name: 'Email',
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
            {
              name: 'Fax',
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
              name: 'FirstName',
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
            {
              name: 'LastName',
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
            {
              name: 'Phone',
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
              name: 'PostalCode',
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
              name: 'State',
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
              name: 'SupportRepId',
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
          name: 'Customer_aggregate',
          description: 'aggregated selection of "Customer"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Customer_aggregate_fields',
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
                      name: 'Customer',
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
          name: 'Customer_aggregate_fields',
          description: 'aggregate fields of "Customer"',
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
                    name: 'Customer_select_column',
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
          name: 'Customer_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Customer". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'number_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'number_SQLite_comparison_exp',
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
                    name: 'Customer_bool_exp',
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
                name: 'Customer_bool_exp',
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
                    name: 'Customer_bool_exp',
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
          name: 'Customer_order_by',
          description: 'Ordering options when selecting data from "Customer".',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
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
          name: 'Customer_select_column',
          description: 'select columns of table "Customer"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'Address',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'City',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Company',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Country',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'CustomerId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Email',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Fax',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'FirstName',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'LastName',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Phone',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'PostalCode',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'State',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'SupportRepId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'SCALAR',
          name: 'Float',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Float_BigQuery_comparison_exp',
          description:
            'Boolean expression to compare columns of type "Float". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
                    name: 'Float',
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
                name: 'Float',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
                    name: 'Float',
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
          name: 'Int',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Int_BigQuery_comparison_exp',
          description:
            'Boolean expression to compare columns of type "Int". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
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
                    name: 'Int',
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
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
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
                    name: 'Int',
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
          name: 'Int_comparison_exp',
          description:
            'Boolean expression to compare columns of type "Int". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
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
                    name: 'Int',
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
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
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
                    name: 'Int',
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
          name: 'Label',
          description: 'columns and relationships of "Label"',
          fields: [
            {
              name: 'City',
              description: 'An object relationship',
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'City',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ParentLabel',
              description: 'An object relationship',
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Label',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'bio',
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
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
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
          name: 'Label_aggregate',
          description: 'aggregated selection of "Label"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Label_aggregate_fields',
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
                      name: 'Label',
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
          name: 'Label_aggregate_fields',
          description: 'aggregate fields of "Label"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Label_avg_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'count',
              description: null,
              args: [
                {
                  name: 'columns',
                  description: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'Label_select_column',
                        ofType: null,
                      },
                    },
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
                name: 'Label_max_fields',
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
                name: 'Label_min_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'stddev',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Label_stddev_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'stddev_pop',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Label_stddev_pop_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'stddev_samp',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Label_stddev_samp_fields',
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
                name: 'Label_sum_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'var_pop',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Label_var_pop_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'var_samp',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Label_var_samp_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'variance',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Label_variance_fields',
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
          name: 'Label_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Label_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Label". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'City',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'City_bool_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ParentLabel',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Label_bool_exp',
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
                    name: 'Label_bool_exp',
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
                name: 'Label_bool_exp',
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
                    name: 'Label_bool_exp',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: 'bio',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
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
          name: 'Label_constraint',
          description: 'unique or primary key constraints on table "Label"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'Label_pkey',
              description: 'unique or primary key constraint on columns "id"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Label_inc_input',
          description:
            'input type for incrementing numeric columns in table "Label"',
          fields: null,
          inputFields: [
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
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
          name: 'Label_insert_input',
          description: 'input type for inserting data into table "Label"',
          fields: null,
          inputFields: [
            {
              name: 'City',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'City_obj_rel_insert_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ParentLabel',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Label_obj_rel_insert_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'bio',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
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
          kind: 'OBJECT',
          name: 'Label_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'bio',
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
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'name',
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
          kind: 'OBJECT',
          name: 'Label_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'bio',
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
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'name',
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
          kind: 'OBJECT',
          name: 'Label_mutation_response',
          description: 'response of any mutation on the table "Label"',
          fields: [
            {
              name: 'affected_rows',
              description: 'number of rows affected by the mutation',
              args: [],
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
              name: 'returning',
              description: 'data from the rows affected by the mutation',
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
                      name: 'Label',
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
          kind: 'INPUT_OBJECT',
          name: 'Label_obj_rel_insert_input',
          description:
            'input type for inserting object relation for remote table "Label"',
          fields: null,
          inputFields: [
            {
              name: 'data',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'Label_insert_input',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
            {
              name: 'on_conflict',
              description: 'upsert condition',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Label_on_conflict',
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
          name: 'Label_on_conflict',
          description: 'on_conflict condition type for table "Label"',
          fields: null,
          inputFields: [
            {
              name: 'constraint',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'ENUM',
                  name: 'Label_constraint',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
            {
              name: 'update_columns',
              description: null,
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
                      kind: 'ENUM',
                      name: 'Label_update_column',
                      ofType: null,
                    },
                  },
                },
              },
              defaultValue: '[]',
            },
            {
              name: 'where',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Label_bool_exp',
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
          name: 'Label_order_by',
          description: 'Ordering options when selecting data from "Label".',
          fields: null,
          inputFields: [
            {
              name: 'City',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'City_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ParentLabel',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Label_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'bio',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
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
          name: 'Label_pk_columns_input',
          description: 'primary key columns input for table: Label',
          fields: null,
          inputFields: [
            {
              name: 'id',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Int',
                  ofType: null,
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
          name: 'Label_select_column',
          description: 'select columns of table "Label"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'bio',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'city_id',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
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
          kind: 'INPUT_OBJECT',
          name: 'Label_set_input',
          description: 'input type for updating data in table "Label"',
          fields: null,
          inputFields: [
            {
              name: 'bio',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
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
          kind: 'OBJECT',
          name: 'Label_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Label_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Label_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Label_stream_cursor_input',
          description: 'Streaming cursor of the table "Label"',
          fields: null,
          inputFields: [
            {
              name: 'initial_value',
              description: 'Stream column input with initial value',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'Label_stream_cursor_value_input',
                  ofType: null,
                },
              },
              defaultValue: null,
            },
            {
              name: 'ordering',
              description: 'cursor ordering',
              type: {
                kind: 'ENUM',
                name: 'cursor_ordering',
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
          name: 'Label_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
          fields: null,
          inputFields: [
            {
              name: 'bio',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'city_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
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
          kind: 'OBJECT',
          name: 'Label_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
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
          name: 'Label_update_column',
          description: 'update columns of table "Label"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'bio',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'city_id',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
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
          kind: 'INPUT_OBJECT',
          name: 'Label_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Label_inc_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Label_set_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'where',
              description: 'filter the rows which have to be updated',
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'Label_bool_exp',
                  ofType: null,
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
          name: 'Label_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Label_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Label_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'city_id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'id',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          kind: 'INPUT_OBJECT',
          name: 'String_BigQuery_comparison_exp',
          description:
            'Boolean expression to compare columns of type "String". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
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
                    name: 'String',
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
              description: 'does the column match the given pattern',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
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
                    name: 'String',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: '_nlike',
              description: 'does the column NOT match the given pattern',
              type: {
                kind: 'SCALAR',
                name: 'String',
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
          name: 'String_comparison_exp',
          description:
            'Boolean expression to compare columns of type "String". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_ilike',
              description:
                'does the column match the given case-insensitive pattern',
              type: {
                kind: 'SCALAR',
                name: 'String',
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
                    name: 'String',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: '_iregex',
              description:
                'does the column match the given POSIX regular expression, case insensitive',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
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
              description: 'does the column match the given pattern',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_nilike',
              description:
                'does the column NOT match the given case-insensitive pattern',
              type: {
                kind: 'SCALAR',
                name: 'String',
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
                    name: 'String',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: '_niregex',
              description:
                'does the column NOT match the given POSIX regular expression, case insensitive',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_nlike',
              description: 'does the column NOT match the given pattern',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_nregex',
              description:
                'does the column NOT match the given POSIX regular expression, case sensitive',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_nsimilar',
              description:
                'does the column NOT match the given SQL regular expression',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_regex',
              description:
                'does the column match the given POSIX regular expression, case sensitive',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_similar',
              description:
                'does the column match the given SQL regular expression',
              type: {
                kind: 'SCALAR',
                name: 'String',
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
          kind: 'OBJECT',
          name: 'bigquery_sample_sample_table',
          description:
            'columns and relationships of "bigquery_sample.sample_table"',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Group',
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
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'STATUS',
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
            {
              name: 'Series_reference',
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
            {
              name: 'Series_title_1',
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
            {
              name: 'Series_title_2',
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
            {
              name: 'Series_title_3',
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
            {
              name: 'Series_title_4',
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
            {
              name: 'Series_title_5',
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
            {
              name: 'Subject',
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
            {
              name: 'Suppressed',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'UNITS',
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
          kind: 'OBJECT',
          name: 'bigquery_sample_sample_table_aggregate',
          description: 'aggregated selection of "bigquery_sample.sample_table"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'bigquery_sample_sample_table_aggregate_fields',
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
                      name: 'bigquery_sample_sample_table',
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
          name: 'bigquery_sample_sample_table_aggregate_fields',
          description: 'aggregate fields of "bigquery_sample.sample_table"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'bigquery_sample_sample_table_avg_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'count',
              description: null,
              args: [
                {
                  name: 'columns',
                  description: null,
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'bigquery_sample_sample_table_select_column',
                        ofType: null,
                      },
                    },
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
                name: 'bigquery_sample_sample_table_max_fields',
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
                name: 'bigquery_sample_sample_table_min_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'stddev',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'bigquery_sample_sample_table_stddev_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'stddev_pop',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'bigquery_sample_sample_table_stddev_pop_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'stddev_samp',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'bigquery_sample_sample_table_stddev_samp_fields',
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
                name: 'bigquery_sample_sample_table_sum_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'var_pop',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'bigquery_sample_sample_table_var_pop_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'var_samp',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'bigquery_sample_sample_table_var_samp_fields',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'variance',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'bigquery_sample_sample_table_variance_fields',
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
          name: 'bigquery_sample_sample_table_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'bigquery_sample_sample_table_bool_exp',
          description:
            'Boolean expression to filter rows from the table "bigquery_sample.sample_table". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'Data_value',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Float_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Group',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Magnitude',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Period',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Float_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'STATUS',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_reference',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_1',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_2',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_3',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_4',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_5',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Subject',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Suppressed',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Boolean_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UNITS',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
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
                    name: 'bigquery_sample_sample_table_bool_exp',
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
                name: 'bigquery_sample_sample_table_bool_exp',
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
                    name: 'bigquery_sample_sample_table_bool_exp',
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
          name: 'bigquery_sample_sample_table_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Group',
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
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'STATUS',
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
            {
              name: 'Series_reference',
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
            {
              name: 'Series_title_1',
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
            {
              name: 'Series_title_2',
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
            {
              name: 'Series_title_3',
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
            {
              name: 'Series_title_4',
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
            {
              name: 'Series_title_5',
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
            {
              name: 'Subject',
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
            {
              name: 'Suppressed',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'UNITS',
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
          kind: 'OBJECT',
          name: 'bigquery_sample_sample_table_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Group',
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
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'STATUS',
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
            {
              name: 'Series_reference',
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
            {
              name: 'Series_title_1',
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
            {
              name: 'Series_title_2',
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
            {
              name: 'Series_title_3',
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
            {
              name: 'Series_title_4',
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
            {
              name: 'Series_title_5',
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
            {
              name: 'Subject',
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
            {
              name: 'Suppressed',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'UNITS',
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
          kind: 'INPUT_OBJECT',
          name: 'bigquery_sample_sample_table_order_by',
          description:
            'Ordering options when selecting data from "bigquery_sample.sample_table".',
          fields: null,
          inputFields: [
            {
              name: 'Data_value',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Group',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Magnitude',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Period',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'STATUS',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_reference',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_1',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_2',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_3',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_4',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Series_title_5',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Subject',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Suppressed',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UNITS',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
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
          name: 'bigquery_sample_sample_table_select_column',
          description: 'select columns of table "bigquery_sample.sample_table"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'Data_value',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Group',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Magnitude',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'STATUS',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Series_reference',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Series_title_1',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Series_title_2',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Series_title_3',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Series_title_4',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Series_title_5',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Subject',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Suppressed',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'UNITS',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'bigquery_sample_sample_table_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'bigquery_sample_sample_table_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'bigquery_sample_sample_table_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'bigquery_sample_sample_table_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'bigquery_sample_sample_table_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'bigquery_sample_sample_table_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'bigquery_sample_sample_table_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'Data_value',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Magnitude',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Period',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'cursor_ordering',
          description: 'ordering argument of a cursor',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'ASC',
              description: 'ascending ordering of the cursor',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'DESC',
              description: 'descending ordering of the cursor',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'mutation_root',
          description: 'mutation root',
          fields: [
            {
              name: 'delete_City',
              description: 'delete data from the table: "City"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'City_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'City_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_City_by_pk',
              description: 'delete single row from the table: "City"',
              args: [
                {
                  name: 'id',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'City',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Label',
              description: 'delete data from the table: "Label"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Label_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Label_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Label_by_pk',
              description: 'delete single row from the table: "Label"',
              args: [
                {
                  name: 'id',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Label',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_City',
              description: 'insert data into the table: "City"',
              args: [
                {
                  name: 'objects',
                  description: 'the rows to be inserted',
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
                          kind: 'INPUT_OBJECT',
                          name: 'City_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
                {
                  name: 'on_conflict',
                  description: 'upsert condition',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'City_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'City_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_City_one',
              description: 'insert a single row into the table: "City"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'City_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
                {
                  name: 'on_conflict',
                  description: 'upsert condition',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'City_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'City',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Label',
              description: 'insert data into the table: "Label"',
              args: [
                {
                  name: 'objects',
                  description: 'the rows to be inserted',
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
                          kind: 'INPUT_OBJECT',
                          name: 'Label_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
                {
                  name: 'on_conflict',
                  description: 'upsert condition',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Label_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Label_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Label_one',
              description: 'insert a single row into the table: "Label"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Label_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
                {
                  name: 'on_conflict',
                  description: 'upsert condition',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Label_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Label',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_City',
              description: 'update data of the table: "City"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'City_inc_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'City_set_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: 'where',
                  description: 'filter the rows which have to be updated',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'City_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'City_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_City_by_pk',
              description: 'update single row of the table: "City"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'City_inc_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'City_set_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: 'pk_columns',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'City_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'City',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_City_many',
              description: 'update multiples rows of table: "City"',
              args: [
                {
                  name: 'updates',
                  description: 'updates to execute, in order',
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
                          kind: 'INPUT_OBJECT',
                          name: 'City_updates',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'City_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Label',
              description: 'update data of the table: "Label"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Label_inc_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Label_set_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: 'where',
                  description: 'filter the rows which have to be updated',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Label_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Label_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Label_by_pk',
              description: 'update single row of the table: "Label"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Label_inc_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Label_set_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: 'pk_columns',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Label_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Label',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Label_many',
              description: 'update multiples rows of table: "Label"',
              args: [
                {
                  name: 'updates',
                  description: 'updates to execute, in order',
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
                          kind: 'INPUT_OBJECT',
                          name: 'Label_updates',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'Label_mutation_response',
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
          kind: 'SCALAR',
          name: 'number',
          description: 'A custom scalar type',
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
          kind: 'ENUM',
          name: 'order_by',
          description: 'column ordering options',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'asc',
              description: 'in ascending order, nulls last',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'asc_nulls_first',
              description: 'in ascending order, nulls first',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'asc_nulls_last',
              description: 'in ascending order, nulls last',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'desc',
              description: 'in descending order, nulls first',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'desc_nulls_first',
              description: 'in descending order, nulls first',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'desc_nulls_last',
              description: 'in descending order, nulls last',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
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
                  description: 'skip the first n rows. Use only with order_by',
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
                  description: 'skip the first n rows. Use only with order_by',
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
                  description: 'skip the first n rows. Use only with order_by',
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
                  description: 'skip the first n rows. Use only with order_by',
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
              name: 'City',
              description: 'fetch data from the table: "City"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'City_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'City_order_by',
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
                    name: 'City_bool_exp',
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
                      name: 'City',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'City_aggregate',
              description: 'fetch aggregated fields from the table: "City"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'City_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'City_order_by',
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
                    name: 'City_bool_exp',
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
                  name: 'City_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'City_by_pk',
              description:
                'fetch data from the table: "City" using primary key columns',
              args: [
                {
                  name: 'id',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'City',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Customer',
              description: 'fetch data from the table: "Customer"',
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'Customer_order_by',
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
                    name: 'Customer_bool_exp',
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
                      name: 'Customer',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Customer_aggregate',
              description: 'fetch aggregated fields from the table: "Customer"',
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'Customer_order_by',
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
                    name: 'Customer_bool_exp',
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
                  name: 'Customer_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Customer_by_pk',
              description:
                'fetch data from the table: "Customer" using primary key columns',
              args: [
                {
                  name: 'CustomerId',
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
                name: 'Customer',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Label',
              description: 'fetch data from the table: "Label"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'Label_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'Label_order_by',
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
                    name: 'Label_bool_exp',
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
                      name: 'Label',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Label_aggregate',
              description: 'fetch aggregated fields from the table: "Label"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'Label_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'Label_order_by',
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
                    name: 'Label_bool_exp',
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
                  name: 'Label_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Label_by_pk',
              description:
                'fetch data from the table: "Label" using primary key columns',
              args: [
                {
                  name: 'id',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Label',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'bigquery_sample_sample_table',
              description:
                'fetch data from the table: "bigquery_sample.sample_table"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'bigquery_sample_sample_table_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'bigquery_sample_sample_table_order_by',
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
                    name: 'bigquery_sample_sample_table_bool_exp',
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
                      name: 'bigquery_sample_sample_table',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'bigquery_sample_sample_table_aggregate',
              description:
                'fetch aggregated fields from the table: "bigquery_sample.sample_table"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'bigquery_sample_sample_table_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'bigquery_sample_sample_table_order_by',
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
                    name: 'bigquery_sample_sample_table_bool_exp',
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
                  name: 'bigquery_sample_sample_table_aggregate',
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
          kind: 'SCALAR',
          name: 'string',
          description: 'A custom scalar type',
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
                  description: 'skip the first n rows. Use only with order_by',
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
                  description: 'skip the first n rows. Use only with order_by',
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
                  description: 'skip the first n rows. Use only with order_by',
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
                  description: 'skip the first n rows. Use only with order_by',
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
              name: 'City',
              description: 'fetch data from the table: "City"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'City_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'City_order_by',
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
                    name: 'City_bool_exp',
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
                      name: 'City',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'City_aggregate',
              description: 'fetch aggregated fields from the table: "City"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'City_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'City_order_by',
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
                    name: 'City_bool_exp',
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
                  name: 'City_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'City_by_pk',
              description:
                'fetch data from the table: "City" using primary key columns',
              args: [
                {
                  name: 'id',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'City',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'City_stream',
              description:
                'fetch data from the table in a streaming manner: "City"',
              args: [
                {
                  name: 'batch_size',
                  description:
                    'maximum number of rows returned in a single batch',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
                {
                  name: 'cursor',
                  description:
                    'cursor to stream the results returned by the query',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'City_stream_cursor_input',
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
                    name: 'City_bool_exp',
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
                      name: 'City',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Customer',
              description: 'fetch data from the table: "Customer"',
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'Customer_order_by',
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
                    name: 'Customer_bool_exp',
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
                      name: 'Customer',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Customer_aggregate',
              description: 'fetch aggregated fields from the table: "Customer"',
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'Customer_order_by',
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
                    name: 'Customer_bool_exp',
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
                  name: 'Customer_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Customer_by_pk',
              description:
                'fetch data from the table: "Customer" using primary key columns',
              args: [
                {
                  name: 'CustomerId',
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
                name: 'Customer',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Label',
              description: 'fetch data from the table: "Label"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'Label_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'Label_order_by',
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
                    name: 'Label_bool_exp',
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
                      name: 'Label',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Label_aggregate',
              description: 'fetch aggregated fields from the table: "Label"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'Label_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'Label_order_by',
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
                    name: 'Label_bool_exp',
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
                  name: 'Label_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Label_by_pk',
              description:
                'fetch data from the table: "Label" using primary key columns',
              args: [
                {
                  name: 'id',
                  description: null,
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Label',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Label_stream',
              description:
                'fetch data from the table in a streaming manner: "Label"',
              args: [
                {
                  name: 'batch_size',
                  description:
                    'maximum number of rows returned in a single batch',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'SCALAR',
                      name: 'Int',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
                {
                  name: 'cursor',
                  description:
                    'cursor to stream the results returned by the query',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'LIST',
                      name: null,
                      ofType: {
                        kind: 'INPUT_OBJECT',
                        name: 'Label_stream_cursor_input',
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
                    name: 'Label_bool_exp',
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
                      name: 'Label',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'bigquery_sample_sample_table',
              description:
                'fetch data from the table: "bigquery_sample.sample_table"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'bigquery_sample_sample_table_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'bigquery_sample_sample_table_order_by',
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
                    name: 'bigquery_sample_sample_table_bool_exp',
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
                      name: 'bigquery_sample_sample_table',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'bigquery_sample_sample_table_aggregate',
              description:
                'fetch aggregated fields from the table: "bigquery_sample.sample_table"',
              args: [
                {
                  name: 'distinct_on',
                  description: 'distinct select on columns',
                  type: {
                    kind: 'LIST',
                    name: null,
                    ofType: {
                      kind: 'NON_NULL',
                      name: null,
                      ofType: {
                        kind: 'ENUM',
                        name: 'bigquery_sample_sample_table_select_column',
                        ofType: null,
                      },
                    },
                  },
                  defaultValue: null,
                },
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
                  description: 'skip the first n rows. Use only with order_by',
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
                        name: 'bigquery_sample_sample_table_order_by',
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
                    name: 'bigquery_sample_sample_table_bool_exp',
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
                  name: 'bigquery_sample_sample_table_aggregate',
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
};
