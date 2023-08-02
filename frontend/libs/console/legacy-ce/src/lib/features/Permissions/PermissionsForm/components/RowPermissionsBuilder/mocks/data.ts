import { buildClientSchema, IntrospectionQuery } from 'graphql';

export const results = {
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
                  name: 'decimal',
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
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Album_avg_fields',
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
              name: 'stddev',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Album_stddev_fields',
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
                name: 'Album_stddev_pop_fields',
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
                name: 'Album_stddev_samp_fields',
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
            {
              name: 'var_pop',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Album_var_pop_fields',
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
                name: 'Album_var_samp_fields',
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
                name: 'Album_variance_fields',
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
          name: 'Album_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'AlbumId',
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
              name: 'ArtistId',
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
                name: 'decimal_Dynamic_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'decimal_Dynamic_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_Dynamic_comparison_exp',
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
                name: 'decimal',
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
                name: 'decimal',
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
          name: 'Album_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'AlbumId',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'decimal',
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
                name: 'decimal',
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
          name: 'Album_order_by',
          description: 'Ordering options when selecting data from "Album".',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'sqlite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'sqlite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'sqlite_order_by',
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
          name: 'Album_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'AlbumId',
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
              name: 'ArtistId',
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
          name: 'Album_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'AlbumId',
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
              name: 'ArtistId',
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
          name: 'Album_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'AlbumId',
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
              name: 'ArtistId',
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
          name: 'Album_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'AlbumId',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'decimal',
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
                name: 'decimal',
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
          name: 'Album_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'AlbumId',
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
              name: 'ArtistId',
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
          name: 'Album_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'AlbumId',
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
              name: 'ArtistId',
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
          name: 'Album_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'AlbumId',
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
              name: 'ArtistId',
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
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Artist_avg_fields',
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
              name: 'stddev',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Artist_stddev_fields',
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
                name: 'Artist_stddev_pop_fields',
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
                name: 'Artist_stddev_samp_fields',
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
            {
              name: 'var_pop',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Artist_var_pop_fields',
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
                name: 'Artist_var_samp_fields',
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
                name: 'Artist_variance_fields',
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
          name: 'Artist_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'ArtistId',
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
                name: 'decimal_Dynamic_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_Dynamic_comparison_exp',
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
                name: 'decimal',
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
          name: 'Artist_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'ArtistId',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'decimal',
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
          name: 'Artist_order_by',
          description: 'Ordering options when selecting data from "Artist".',
          fields: null,
          inputFields: [
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'sqlite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'sqlite_order_by',
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
          name: 'Artist_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'ArtistId',
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
          name: 'Artist_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'ArtistId',
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
          name: 'Artist_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'ArtistId',
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
          name: 'Artist_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'ArtistId',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'decimal',
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
          name: 'Artist_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'ArtistId',
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
          name: 'Artist_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'ArtistId',
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
          name: 'Artist_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'ArtistId',
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
          name: 'Float',
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
          name: 'String_Dynamic_comparison_exp',
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
          kind: 'SCALAR',
          name: 'decimal',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'decimal_Dynamic_comparison_exp',
          description:
            'Boolean expression to compare columns of type "decimal". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'decimal',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'decimal',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'decimal',
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
                    name: 'decimal',
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
                name: 'decimal',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'decimal',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'decimal',
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
                    name: 'decimal',
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
          name: 'float8',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'float8_comparison_exp',
          description:
            'Boolean expression to compare columns of type "float8". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'float8',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'float8',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'float8',
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
                    name: 'float8',
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
                name: 'float8',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'float8',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'float8',
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
                    name: 'float8',
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
          name: 'mutation_root',
          description: 'mutation root',
          fields: [
            {
              name: 'delete_thing',
              description: 'delete data from the table: "thing"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'thing_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'thing_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_thing_by_pk',
              description: 'delete single row from the table: "thing"',
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
                name: 'thing',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_user',
              description: 'delete data from the table: "user"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'user_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'user_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_user_by_pk',
              description: 'delete single row from the table: "user"',
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
                name: 'user',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_thing',
              description: 'insert data into the table: "thing"',
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
                          name: 'thing_insert_input',
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
                    name: 'thing_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'thing_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_thing_one',
              description: 'insert a single row into the table: "thing"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'thing_insert_input',
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
                    name: 'thing_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'thing',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_user',
              description: 'insert data into the table: "user"',
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
                          name: 'user_insert_input',
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
                    name: 'user_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'user_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_user_one',
              description: 'insert a single row into the table: "user"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'user_insert_input',
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
                    name: 'user_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'user',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_thing',
              description: 'update data of the table: "thing"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'thing_inc_input',
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
                    name: 'thing_set_input',
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
                      name: 'thing_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'thing_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_thing_by_pk',
              description: 'update single row of the table: "thing"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'thing_inc_input',
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
                    name: 'thing_set_input',
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
                      name: 'thing_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'thing',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_thing_many',
              description: 'update multiples rows of table: "thing"',
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
                          name: 'thing_updates',
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
                  name: 'thing_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_user',
              description: 'update data of the table: "user"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_inc_input',
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
                    name: 'user_set_input',
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
                      name: 'user_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'user_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_user_by_pk',
              description: 'update single row of the table: "user"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'user_inc_input',
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
                    name: 'user_set_input',
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
                      name: 'user_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'user',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_user_many',
              description: 'update multiples rows of table: "user"',
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
                          name: 'user_updates',
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
                  name: 'user_mutation_response',
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
                      name: 'decimal',
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
                      name: 'decimal',
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
              name: 'thing',
              description: 'fetch data from the table: "thing"',
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
                        name: 'thing_select_column',
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
                        name: 'thing_order_by',
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
                    name: 'thing_bool_exp',
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
                      name: 'thing',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'thing_aggregate',
              description: 'fetch aggregated fields from the table: "thing"',
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
                        name: 'thing_select_column',
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
                        name: 'thing_order_by',
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
                    name: 'thing_bool_exp',
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
                  name: 'thing_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'thing_by_pk',
              description:
                'fetch data from the table: "thing" using primary key columns',
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
                name: 'thing',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'user',
              description: 'fetch data from the table: "user"',
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
                        name: 'user_select_column',
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
                        name: 'user_order_by',
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
                    name: 'user_bool_exp',
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
                      name: 'user',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'user_aggregate',
              description: 'fetch aggregated fields from the table: "user"',
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
                        name: 'user_select_column',
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
                        name: 'user_order_by',
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
                    name: 'user_bool_exp',
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
                  name: 'user_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'user_by_pk',
              description:
                'fetch data from the table: "user" using primary key columns',
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
                name: 'user',
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
          name: 'sqlite_order_by',
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
                      name: 'decimal',
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
                      name: 'decimal',
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
              name: 'thing',
              description: 'fetch data from the table: "thing"',
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
                        name: 'thing_select_column',
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
                        name: 'thing_order_by',
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
                    name: 'thing_bool_exp',
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
                      name: 'thing',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'thing_aggregate',
              description: 'fetch aggregated fields from the table: "thing"',
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
                        name: 'thing_select_column',
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
                        name: 'thing_order_by',
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
                    name: 'thing_bool_exp',
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
                  name: 'thing_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'thing_by_pk',
              description:
                'fetch data from the table: "thing" using primary key columns',
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
                name: 'thing',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'thing_stream',
              description:
                'fetch data from the table in a streaming manner: "thing"',
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
                        name: 'thing_stream_cursor_input',
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
                    name: 'thing_bool_exp',
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
                      name: 'thing',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'user',
              description: 'fetch data from the table: "user"',
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
                        name: 'user_select_column',
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
                        name: 'user_order_by',
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
                    name: 'user_bool_exp',
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
                      name: 'user',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'user_aggregate',
              description: 'fetch aggregated fields from the table: "user"',
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
                        name: 'user_select_column',
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
                        name: 'user_order_by',
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
                    name: 'user_bool_exp',
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
                  name: 'user_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'user_by_pk',
              description:
                'fetch data from the table: "user" using primary key columns',
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
                name: 'user',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'user_stream',
              description:
                'fetch data from the table in a streaming manner: "user"',
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
                        name: 'user_stream_cursor_input',
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
                    name: 'user_bool_exp',
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
                      name: 'user',
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
          name: 'thing',
          description: 'columns and relationships of "thing"',
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
              name: 'fk_user_id',
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
            {
              name: 'user',
              description: 'An object relationship',
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'OBJECT',
                  name: 'user',
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
          name: 'thing_aggregate',
          description: 'aggregated selection of "thing"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'thing_aggregate_fields',
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
                      name: 'thing',
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
          name: 'thing_aggregate_fields',
          description: 'aggregate fields of "thing"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'thing_avg_fields',
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
                        name: 'thing_select_column',
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
                name: 'thing_max_fields',
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
                name: 'thing_min_fields',
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
                name: 'thing_stddev_fields',
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
                name: 'thing_stddev_pop_fields',
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
                name: 'thing_stddev_samp_fields',
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
                name: 'thing_sum_fields',
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
                name: 'thing_var_pop_fields',
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
                name: 'thing_var_samp_fields',
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
                name: 'thing_variance_fields',
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
          name: 'thing_aggregate_order_by',
          description: 'order by aggregate values of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'avg',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_avg_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'count',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'max',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_max_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'min',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_min_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'stddev',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_stddev_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'stddev_pop',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_stddev_pop_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'stddev_samp',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_stddev_samp_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'sum',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_sum_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'var_pop',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_var_pop_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'var_samp',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_var_samp_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'variance',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_variance_order_by',
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
          name: 'thing_arr_rel_insert_input',
          description:
            'input type for inserting array relation for remote table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'data',
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
                      kind: 'INPUT_OBJECT',
                      name: 'thing_insert_input',
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
                name: 'thing_on_conflict',
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
          name: 'thing_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'fk_user_id',
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
          name: 'thing_avg_order_by',
          description: 'order by avg() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'fk_user_id',
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
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'thing_bool_exp',
          description:
            'Boolean expression to filter rows from the table "thing". All fields are combined with a logical \'AND\'.',
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
                    name: 'thing_bool_exp',
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
                name: 'thing_bool_exp',
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
                    name: 'thing_bool_exp',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: 'description',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'fk_user_id',
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
            {
              name: 'user',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_bool_exp',
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
          name: 'thing_constraint',
          description: 'unique or primary key constraints on table "thing"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'thing_pkey',
              description: 'unique or primary key constraint on columns "id"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'thing_inc_input',
          description:
            'input type for incrementing numeric columns in table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'fk_user_id',
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
          name: 'thing_insert_input',
          description: 'input type for inserting data into table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'description',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'fk_user_id',
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
            {
              name: 'user',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_obj_rel_insert_input',
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
          name: 'thing_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'description',
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
              name: 'fk_user_id',
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
          kind: 'INPUT_OBJECT',
          name: 'thing_max_order_by',
          description: 'order by max() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'description',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'fk_user_id',
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
          kind: 'OBJECT',
          name: 'thing_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'description',
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
              name: 'fk_user_id',
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
          kind: 'INPUT_OBJECT',
          name: 'thing_min_order_by',
          description: 'order by min() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'description',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'fk_user_id',
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
          kind: 'OBJECT',
          name: 'thing_mutation_response',
          description: 'response of any mutation on the table "thing"',
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
                      name: 'thing',
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
          name: 'thing_on_conflict',
          description: 'on_conflict condition type for table "thing"',
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
                  name: 'thing_constraint',
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
                      name: 'thing_update_column',
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
                name: 'thing_bool_exp',
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
          name: 'thing_order_by',
          description: 'Ordering options when selecting data from "thing".',
          fields: null,
          inputFields: [
            {
              name: 'description',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'fk_user_id',
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
            {
              name: 'user',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_order_by',
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
          name: 'thing_pk_columns_input',
          description: 'primary key columns input for table: thing',
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
          name: 'thing_select_column',
          description: 'select columns of table "thing"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'description',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'fk_user_id',
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
          name: 'thing_set_input',
          description: 'input type for updating data in table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'description',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'fk_user_id',
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
          name: 'thing_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'fk_user_id',
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
          name: 'thing_stddev_order_by',
          description: 'order by stddev() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'fk_user_id',
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
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'thing_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'fk_user_id',
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
          name: 'thing_stddev_pop_order_by',
          description: 'order by stddev_pop() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'fk_user_id',
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
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'thing_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'fk_user_id',
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
          name: 'thing_stddev_samp_order_by',
          description: 'order by stddev_samp() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'fk_user_id',
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
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'thing_stream_cursor_input',
          description: 'Streaming cursor of the table "thing"',
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
                  name: 'thing_stream_cursor_value_input',
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
          name: 'thing_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
          fields: null,
          inputFields: [
            {
              name: 'description',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'fk_user_id',
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
          name: 'thing_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'fk_user_id',
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
                isDeprecated: false,
              },
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
          name: 'thing_sum_order_by',
          description: 'order by sum() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'fk_user_id',
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
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'ENUM',
          name: 'thing_update_column',
          description: 'update columns of table "thing"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'description',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'fk_user_id',
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
          name: 'thing_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_inc_input',
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
                name: 'thing_set_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'where',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'thing_bool_exp',
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
          name: 'thing_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'fk_user_id',
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
          name: 'thing_var_pop_order_by',
          description: 'order by var_pop() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'fk_user_id',
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
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'thing_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'fk_user_id',
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
          name: 'thing_var_samp_order_by',
          description: 'order by var_samp() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'fk_user_id',
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
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'thing_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'fk_user_id',
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
          name: 'thing_variance_order_by',
          description: 'order by variance() on columns of table "thing"',
          fields: null,
          inputFields: [
            {
              name: 'fk_user_id',
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
          ],
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'user',
          description: 'columns and relationships of "user"',
          fields: [
            {
              name: 'age',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'float8',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'email',
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
            {
              name: 'things',
              description: 'An array relationship',
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
                        name: 'thing_select_column',
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
                        name: 'thing_order_by',
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
                    name: 'thing_bool_exp',
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
                      name: 'thing',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'things_aggregate',
              description: 'An aggregate relationship',
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
                        name: 'thing_select_column',
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
                        name: 'thing_order_by',
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
                    name: 'thing_bool_exp',
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
                  name: 'thing_aggregate',
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
          name: 'user_aggregate',
          description: 'aggregated selection of "user"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'user_aggregate_fields',
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
                      name: 'user',
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
          name: 'user_aggregate_fields',
          description: 'aggregate fields of "user"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'user_avg_fields',
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
                        name: 'user_select_column',
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
                name: 'user_max_fields',
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
                name: 'user_min_fields',
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
                name: 'user_stddev_fields',
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
                name: 'user_stddev_pop_fields',
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
                name: 'user_stddev_samp_fields',
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
                name: 'user_sum_fields',
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
                name: 'user_var_pop_fields',
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
                name: 'user_var_samp_fields',
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
                name: 'user_variance_fields',
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
          name: 'user_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'age',
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
          name: 'user_bool_exp',
          description:
            'Boolean expression to filter rows from the table "user". All fields are combined with a logical \'AND\'.',
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
                    name: 'user_bool_exp',
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
                name: 'user_bool_exp',
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
                    name: 'user_bool_exp',
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
                name: 'float8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'email',
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
            {
              name: 'things',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_bool_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'things_aggregate',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_things_aggregate',
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
          name: 'user_constraint',
          description: 'unique or primary key constraints on table "user"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'user_pkey',
              description: 'unique or primary key constraint on columns "id"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'user_inc_input',
          description:
            'input type for incrementing numeric columns in table "user"',
          fields: null,
          inputFields: [
            {
              name: 'age',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'float8',
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
          name: 'user_insert_input',
          description: 'input type for inserting data into table "user"',
          fields: null,
          inputFields: [
            {
              name: 'age',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'float8',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'email',
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
            {
              name: 'things',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_arr_rel_insert_input',
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
          name: 'user_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'age',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'float8',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'email',
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
          name: 'user_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'age',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'float8',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'email',
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
          name: 'user_mutation_response',
          description: 'response of any mutation on the table "user"',
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
                      name: 'user',
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
          name: 'user_obj_rel_insert_input',
          description:
            'input type for inserting object relation for remote table "user"',
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
                  name: 'user_insert_input',
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
                name: 'user_on_conflict',
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
          name: 'user_on_conflict',
          description: 'on_conflict condition type for table "user"',
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
                  name: 'user_constraint',
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
                      name: 'user_update_column',
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
                name: 'user_bool_exp',
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
          name: 'user_order_by',
          description: 'Ordering options when selecting data from "user".',
          fields: null,
          inputFields: [
            {
              name: 'age',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'email',
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
            {
              name: 'things_aggregate',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_aggregate_order_by',
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
          name: 'user_pk_columns_input',
          description: 'primary key columns input for table: user',
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
          name: 'user_select_column',
          description: 'select columns of table "user"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
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
          name: 'user_set_input',
          description: 'input type for updating data in table "user"',
          fields: null,
          inputFields: [
            {
              name: 'age',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'float8',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'email',
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
          name: 'user_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'age',
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
          name: 'user_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'age',
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
          name: 'user_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'age',
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
          name: 'user_stream_cursor_input',
          description: 'Streaming cursor of the table "user"',
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
                  name: 'user_stream_cursor_value_input',
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
          name: 'user_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
          fields: null,
          inputFields: [
            {
              name: 'age',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'float8',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'email',
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
          name: 'user_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'age',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'float8',
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
          kind: 'INPUT_OBJECT',
          name: 'user_things_aggregate',
          description: null,
          fields: null,
          inputFields: [
            {
              name: 'count',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_things_aggregate_count',
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
          name: 'user_things_aggregate_count',
          description: null,
          fields: null,
          inputFields: [
            {
              name: 'arguments',
              description: null,
              type: {
                kind: 'LIST',
                name: null,
                ofType: {
                  kind: 'NON_NULL',
                  name: null,
                  ofType: {
                    kind: 'ENUM',
                    name: 'thing_select_column',
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
            {
              name: 'filter',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'thing_bool_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'predicate',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'Int_comparison_exp',
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
          name: 'user_update_column',
          description: 'update columns of table "user"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
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
          name: 'user_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'user_inc_input',
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
                name: 'user_set_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'where',
              description: null,
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'INPUT_OBJECT',
                  name: 'user_bool_exp',
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
          name: 'user_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'age',
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
          name: 'user_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'age',
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
          name: 'user_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'age',
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

export const schema = buildClientSchema(
  results.data as unknown as IntrospectionQuery
);
