import { buildClientSchema, GraphQLSchema, IntrospectionQuery } from 'graphql';

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
          name: 'ALBUM',
          description: 'columns and relationships of "ALBUM"',
          fields: [
            {
              name: 'ALBUMID',
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
              name: 'ARTISTID',
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
              name: 'TITLE',
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
          name: 'ALBUM_aggregate',
          description: 'aggregated selection of "ALBUM"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'ALBUM_aggregate_fields',
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
                      name: 'ALBUM',
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
          name: 'ALBUM_aggregate_fields',
          description: 'aggregate fields of "ALBUM"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'ALBUM_avg_fields',
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
                    name: 'ALBUM_select_column',
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
                name: 'ALBUM_max_fields',
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
                name: 'ALBUM_min_fields',
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
                name: 'ALBUM_stddev_pop_fields',
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
                name: 'ALBUM_stddev_samp_fields',
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
                name: 'ALBUM_sum_fields',
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
                name: 'ALBUM_var_pop_fields',
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
                name: 'ALBUM_var_samp_fields',
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
          name: 'ALBUM_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'ALBUMID',
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
              name: 'ARTISTID',
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
          name: 'ALBUM_bool_exp',
          description:
            'Boolean expression to filter rows from the table "ALBUM". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'ALBUMID',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_snowflake_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ARTISTID',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_snowflake_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TITLE',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_snowflake_comparison_exp',
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
                    name: 'ALBUM_bool_exp',
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
                name: 'ALBUM_bool_exp',
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
                    name: 'ALBUM_bool_exp',
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
          name: 'ALBUM_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'ALBUMID',
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
              name: 'ARTISTID',
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
              name: 'TITLE',
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
          name: 'ALBUM_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'ALBUMID',
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
              name: 'ARTISTID',
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
              name: 'TITLE',
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
          name: 'ALBUM_order_by',
          description: 'Ordering options when selecting data from "ALBUM".',
          fields: null,
          inputFields: [
            {
              name: 'ALBUMID',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'snowflake_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ARTISTID',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'snowflake_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TITLE',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'snowflake_order_by',
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
          name: 'ALBUM_select_column',
          description: 'select columns of table "ALBUM"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'ALBUMID',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ARTISTID',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'TITLE',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'ALBUM_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'ALBUMID',
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
              name: 'ARTISTID',
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
          name: 'ALBUM_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'ALBUMID',
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
              name: 'ARTISTID',
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
          name: 'ALBUM_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'ALBUMID',
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
              name: 'ARTISTID',
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
          kind: 'OBJECT',
          name: 'ALBUM_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'ALBUMID',
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
              name: 'ARTISTID',
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
          name: 'ALBUM_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'ALBUMID',
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
              name: 'ARTISTID',
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
          name: 'Album',
          description: 'columns and relationships of "Album"',
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
                  name: 'Int',
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
                  name: 'Int',
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
                        name: 'Album_select_column',
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
                name: 'Int_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
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
          kind: 'ENUM',
          name: 'Album_constraint',
          description: 'unique or primary key constraints on table "Album"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'PK_Album',
              description:
                'unique or primary key constraint on columns "AlbumId"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Album_inc_input',
          description:
            'input type for incrementing numeric columns in table "Album"',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
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
          name: 'Album_insert_input',
          description: 'input type for inserting data into table "Album"',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
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
          name: 'Album_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'AlbumId',
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
              name: 'ArtistId',
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
                name: 'Int',
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
                name: 'Int',
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
          name: 'Album_mutation_response',
          description: 'response of any mutation on the table "Album"',
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
          kind: 'INPUT_OBJECT',
          name: 'Album_on_conflict',
          description: 'on_conflict condition type for table "Album"',
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
                  name: 'Album_constraint',
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
                      name: 'Album_update_column',
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
                name: 'Album_bool_exp',
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
          name: 'Album_order_by',
          description: 'Ordering options when selecting data from "Album".',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
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
          name: 'Album_pk_columns_input',
          description: 'primary key columns input for table: Album',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
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
          kind: 'INPUT_OBJECT',
          name: 'Album_set_input',
          description: 'input type for updating data in table "Album"',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
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
          kind: 'INPUT_OBJECT',
          name: 'Album_stream_cursor_input',
          description: 'Streaming cursor of the table "Album"',
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
                  name: 'Album_stream_cursor_value_input',
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
          name: 'Album_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
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
          name: 'Album_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'AlbumId',
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
              name: 'ArtistId',
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
          name: 'Album_update_column',
          description: 'update columns of table "Album"',
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
          kind: 'INPUT_OBJECT',
          name: 'Album_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Album_inc_input',
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
                name: 'Album_set_input',
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
                  name: 'Album_bool_exp',
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
          kind: 'OBJECT',
          name: 'Chinook_Album',
          description: '',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'ArtistId',
              description: '',
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
              name: 'Title',
              description: '',
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
          name: 'Chinook_Album_aggregate',
          description: 'aggregated selection of "Chinook.Album"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Album_aggregate_fields',
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
                      name: 'Chinook_Album',
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
          name: 'Chinook_Album_aggregate_fields',
          description: 'aggregate fields of "Chinook.Album"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Album_avg_fields',
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
                    name: 'Chinook_Album_select_column',
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
                name: 'Chinook_Album_max_fields',
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
                name: 'Chinook_Album_min_fields',
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
                name: 'Chinook_Album_stddev_pop_fields',
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
                name: 'Chinook_Album_stddev_samp_fields',
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
                name: 'Chinook_Album_sum_fields',
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
                name: 'Chinook_Album_var_pop_fields',
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
                name: 'Chinook_Album_var_samp_fields',
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
          name: 'Chinook_Album_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              description: '',
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
          name: 'Chinook_Album_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.Album". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
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
                    name: 'Chinook_Album_bool_exp',
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
                name: 'Chinook_Album_bool_exp',
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
                    name: 'Chinook_Album_bool_exp',
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
          name: 'Chinook_Album_insert_input',
          description:
            'input type for inserting data into table "Chinook.Album"',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: '',
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
          name: 'Chinook_Album_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'ArtistId',
              description: '',
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
              name: 'Title',
              description: '',
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
          name: 'Chinook_Album_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'ArtistId',
              description: '',
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
              name: 'Title',
              description: '',
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
          name: 'Chinook_Album_mutation_response',
          description: 'response of any mutation on the table "Chinook.Album"',
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
                      name: 'Chinook_Album',
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
          name: 'Chinook_Album_order_by',
          description:
            'Ordering options when selecting data from "Chinook.Album".',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_Album_pk_columns_input',
          description: 'primary key columns input for table: Chinook.Album',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: '',
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
          name: 'Chinook_Album_select_column',
          description: 'select columns of table "Chinook.Album"',
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
          kind: 'INPUT_OBJECT',
          name: 'Chinook_Album_set_input',
          description: 'input type for updating data in table "Chinook.Album"',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ArtistId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: '',
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
          name: 'Chinook_Album_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              description: '',
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
          name: 'Chinook_Album_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              description: '',
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
          name: 'Chinook_Album_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'ArtistId',
              description: '',
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
          name: 'Chinook_Album_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_Album_set_input',
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
                  name: 'Chinook_Album_bool_exp',
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
          name: 'Chinook_Album_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              description: '',
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
          name: 'Chinook_Album_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              description: '',
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
          name: 'Chinook_Artist',
          description: '',
          fields: [
            {
              name: 'Album_Artist',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Album',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ArtistId',
              description: '',
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
              name: 'Name',
              description: '',
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
          name: 'Chinook_Artist_aggregate',
          description: 'aggregated selection of "Chinook.Artist"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Artist_aggregate_fields',
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
                      name: 'Chinook_Artist',
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
          name: 'Chinook_Artist_aggregate_fields',
          description: 'aggregate fields of "Chinook.Artist"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Artist_avg_fields',
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
                    name: 'Chinook_Artist_select_column',
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
                name: 'Chinook_Artist_max_fields',
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
                name: 'Chinook_Artist_min_fields',
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
                name: 'Chinook_Artist_stddev_pop_fields',
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
                name: 'Chinook_Artist_stddev_samp_fields',
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
                name: 'Chinook_Artist_sum_fields',
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
                name: 'Chinook_Artist_var_pop_fields',
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
                name: 'Chinook_Artist_var_samp_fields',
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
          name: 'Chinook_Artist_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'ArtistId',
              description: '',
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
          name: 'Chinook_Artist_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.Artist". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
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
                    name: 'Chinook_Artist_bool_exp',
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
                name: 'Chinook_Artist_bool_exp',
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
                    name: 'Chinook_Artist_bool_exp',
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
          name: 'Chinook_Artist_insert_input',
          description:
            'input type for inserting data into table "Chinook.Artist"',
          fields: null,
          inputFields: [
            {
              name: 'ArtistId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: '',
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
          name: 'Chinook_Artist_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'ArtistId',
              description: '',
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
              name: 'Name',
              description: '',
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
          name: 'Chinook_Artist_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'ArtistId',
              description: '',
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
              name: 'Name',
              description: '',
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
          name: 'Chinook_Artist_mutation_response',
          description: 'response of any mutation on the table "Chinook.Artist"',
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
                      name: 'Chinook_Artist',
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
          name: 'Chinook_Artist_order_by',
          description:
            'Ordering options when selecting data from "Chinook.Artist".',
          fields: null,
          inputFields: [
            {
              name: 'ArtistId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_Artist_pk_columns_input',
          description: 'primary key columns input for table: Chinook.Artist',
          fields: null,
          inputFields: [
            {
              name: 'ArtistId',
              description: '',
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
          name: 'Chinook_Artist_select_column',
          description: 'select columns of table "Chinook.Artist"',
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
          kind: 'INPUT_OBJECT',
          name: 'Chinook_Artist_set_input',
          description: 'input type for updating data in table "Chinook.Artist"',
          fields: null,
          inputFields: [
            {
              name: 'ArtistId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: '',
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
          name: 'Chinook_Artist_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'ArtistId',
              description: '',
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
          name: 'Chinook_Artist_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'ArtistId',
              description: '',
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
          name: 'Chinook_Artist_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'ArtistId',
              description: '',
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
          name: 'Chinook_Artist_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_Artist_set_input',
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
                  name: 'Chinook_Artist_bool_exp',
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
          name: 'Chinook_Artist_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'ArtistId',
              description: '',
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
          name: 'Chinook_Artist_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'ArtistId',
              description: '',
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
          name: 'Chinook_Customer',
          description: '',
          fields: [
            {
              name: 'Address',
              description: '',
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
              name: 'City',
              description: '',
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
              name: 'Company',
              description: '',
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
              name: 'Country',
              description: '',
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
              name: 'CustomerId',
              description: '',
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
              name: 'Email',
              description: '',
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
              name: 'Fax',
              description: '',
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
              name: 'FirstName',
              description: '',
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
              name: 'LastName',
              description: '',
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
              name: 'Phone',
              description: '',
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
              name: 'PostalCode',
              description: '',
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
              name: 'State',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_Customer_aggregate',
          description: 'aggregated selection of "Chinook.Customer"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Customer_aggregate_fields',
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
                      name: 'Chinook_Customer',
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
          name: 'Chinook_Customer_aggregate_fields',
          description: 'aggregate fields of "Chinook.Customer"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Customer_avg_fields',
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
                    name: 'Chinook_Customer_select_column',
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
                name: 'Chinook_Customer_max_fields',
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
                name: 'Chinook_Customer_min_fields',
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
                name: 'Chinook_Customer_stddev_pop_fields',
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
                name: 'Chinook_Customer_stddev_samp_fields',
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
                name: 'Chinook_Customer_sum_fields',
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
                name: 'Chinook_Customer_var_pop_fields',
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
                name: 'Chinook_Customer_var_samp_fields',
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
          name: 'Chinook_Customer_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Chinook_Customer_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.Customer". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
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
                    name: 'Chinook_Customer_bool_exp',
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
                name: 'Chinook_Customer_bool_exp',
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
                    name: 'Chinook_Customer_bool_exp',
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
          name: 'Chinook_Customer_insert_input',
          description:
            'input type for inserting data into table "Chinook.Customer"',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_Customer_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'Address',
              description: '',
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
              name: 'City',
              description: '',
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
              name: 'Company',
              description: '',
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
              name: 'Country',
              description: '',
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
              name: 'CustomerId',
              description: '',
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
              name: 'Email',
              description: '',
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
              name: 'Fax',
              description: '',
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
              name: 'FirstName',
              description: '',
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
              name: 'LastName',
              description: '',
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
              name: 'Phone',
              description: '',
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
              name: 'PostalCode',
              description: '',
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
              name: 'State',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_Customer_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'Address',
              description: '',
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
              name: 'City',
              description: '',
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
              name: 'Company',
              description: '',
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
              name: 'Country',
              description: '',
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
              name: 'CustomerId',
              description: '',
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
              name: 'Email',
              description: '',
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
              name: 'Fax',
              description: '',
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
              name: 'FirstName',
              description: '',
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
              name: 'LastName',
              description: '',
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
              name: 'Phone',
              description: '',
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
              name: 'PostalCode',
              description: '',
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
              name: 'State',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_Customer_mutation_response',
          description:
            'response of any mutation on the table "Chinook.Customer"',
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
                      name: 'Chinook_Customer',
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
          name: 'Chinook_Customer_order_by',
          description:
            'Ordering options when selecting data from "Chinook.Customer".',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_Customer_pk_columns_input',
          description: 'primary key columns input for table: Chinook.Customer',
          fields: null,
          inputFields: [
            {
              name: 'CustomerId',
              description: '',
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
          name: 'Chinook_Customer_select_column',
          description: 'select columns of table "Chinook.Customer"',
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
          kind: 'INPUT_OBJECT',
          name: 'Chinook_Customer_set_input',
          description:
            'input type for updating data in table "Chinook.Customer"',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_Customer_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Chinook_Customer_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Chinook_Customer_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Chinook_Customer_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_Customer_set_input',
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
                  name: 'Chinook_Customer_bool_exp',
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
          name: 'Chinook_Customer_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Chinook_Customer_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Chinook_Employee',
          description: '',
          fields: [
            {
              name: 'Address',
              description: '',
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
              name: 'BirthDate',
              description: '',
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'City',
              description: '',
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
              name: 'Country',
              description: '',
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
              name: 'Email',
              description: '',
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
              name: 'EmployeeId',
              description: '',
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
              name: 'Fax',
              description: '',
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
              name: 'FirstName',
              description: '',
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
              name: 'HireDate',
              description: '',
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'LastName',
              description: '',
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
              name: 'Phone',
              description: '',
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
              name: 'PostalCode',
              description: '',
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
              name: 'ReportsTo',
              description: '',
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
              name: 'State',
              description: '',
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
              name: 'Title',
              description: '',
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
          name: 'Chinook_Employee_aggregate',
          description: 'aggregated selection of "Chinook.Employee"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Employee_aggregate_fields',
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
                      name: 'Chinook_Employee',
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
          name: 'Chinook_Employee_aggregate_fields',
          description: 'aggregate fields of "Chinook.Employee"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Employee_avg_fields',
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
                    name: 'Chinook_Employee_select_column',
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
                name: 'Chinook_Employee_max_fields',
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
                name: 'Chinook_Employee_min_fields',
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
                name: 'Chinook_Employee_stddev_pop_fields',
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
                name: 'Chinook_Employee_stddev_samp_fields',
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
                name: 'Chinook_Employee_sum_fields',
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
                name: 'Chinook_Employee_var_pop_fields',
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
                name: 'Chinook_Employee_var_samp_fields',
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
          name: 'Chinook_Employee_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'EmployeeId',
              description: '',
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
              name: 'ReportsTo',
              description: '',
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
          name: 'Chinook_Employee_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.Employee". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BirthDate',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'datetime_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'EmployeeId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'HireDate',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'datetime_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ReportsTo',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
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
                    name: 'Chinook_Employee_bool_exp',
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
                name: 'Chinook_Employee_bool_exp',
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
                    name: 'Chinook_Employee_bool_exp',
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
          name: 'Chinook_Employee_insert_input',
          description:
            'input type for inserting data into table "Chinook.Employee"',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BirthDate',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'EmployeeId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'HireDate',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ReportsTo',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: '',
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
          name: 'Chinook_Employee_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'Address',
              description: '',
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
              name: 'City',
              description: '',
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
              name: 'Country',
              description: '',
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
              name: 'Email',
              description: '',
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
              name: 'EmployeeId',
              description: '',
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
              name: 'Fax',
              description: '',
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
              name: 'FirstName',
              description: '',
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
              name: 'LastName',
              description: '',
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
              name: 'Phone',
              description: '',
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
              name: 'PostalCode',
              description: '',
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
              name: 'ReportsTo',
              description: '',
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
              name: 'State',
              description: '',
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
              name: 'Title',
              description: '',
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
          name: 'Chinook_Employee_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'Address',
              description: '',
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
              name: 'City',
              description: '',
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
              name: 'Country',
              description: '',
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
              name: 'Email',
              description: '',
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
              name: 'EmployeeId',
              description: '',
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
              name: 'Fax',
              description: '',
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
              name: 'FirstName',
              description: '',
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
              name: 'LastName',
              description: '',
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
              name: 'Phone',
              description: '',
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
              name: 'PostalCode',
              description: '',
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
              name: 'ReportsTo',
              description: '',
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
              name: 'State',
              description: '',
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
              name: 'Title',
              description: '',
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
          name: 'Chinook_Employee_mutation_response',
          description:
            'response of any mutation on the table "Chinook.Employee"',
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
                      name: 'Chinook_Employee',
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
          name: 'Chinook_Employee_order_by',
          description:
            'Ordering options when selecting data from "Chinook.Employee".',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BirthDate',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'EmployeeId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'HireDate',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ReportsTo',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_Employee_pk_columns_input',
          description: 'primary key columns input for table: Chinook.Employee',
          fields: null,
          inputFields: [
            {
              name: 'EmployeeId',
              description: '',
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
          name: 'Chinook_Employee_select_column',
          description: 'select columns of table "Chinook.Employee"',
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
              name: 'BirthDate',
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
              name: 'Country',
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
              name: 'EmployeeId',
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
              name: 'HireDate',
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
              name: 'ReportsTo',
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
              name: 'Title',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Chinook_Employee_set_input',
          description:
            'input type for updating data in table "Chinook.Employee"',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BirthDate',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'EmployeeId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'HireDate',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'ReportsTo',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Title',
              description: '',
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
          name: 'Chinook_Employee_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'EmployeeId',
              description: '',
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
              name: 'ReportsTo',
              description: '',
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
          name: 'Chinook_Employee_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'EmployeeId',
              description: '',
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
              name: 'ReportsTo',
              description: '',
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
          name: 'Chinook_Employee_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'EmployeeId',
              description: '',
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
              name: 'ReportsTo',
              description: '',
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
          name: 'Chinook_Employee_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_Employee_set_input',
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
                  name: 'Chinook_Employee_bool_exp',
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
          name: 'Chinook_Employee_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'EmployeeId',
              description: '',
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
              name: 'ReportsTo',
              description: '',
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
          name: 'Chinook_Employee_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'EmployeeId',
              description: '',
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
              name: 'ReportsTo',
              description: '',
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
          name: 'Chinook_Genre',
          description: '',
          fields: [
            {
              name: 'GenreId',
              description: '',
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
              name: 'Name',
              description: '',
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
          name: 'Chinook_Genre_aggregate',
          description: 'aggregated selection of "Chinook.Genre"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Genre_aggregate_fields',
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
                      name: 'Chinook_Genre',
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
          name: 'Chinook_Genre_aggregate_fields',
          description: 'aggregate fields of "Chinook.Genre"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Genre_avg_fields',
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
                    name: 'Chinook_Genre_select_column',
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
                name: 'Chinook_Genre_max_fields',
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
                name: 'Chinook_Genre_min_fields',
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
                name: 'Chinook_Genre_stddev_pop_fields',
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
                name: 'Chinook_Genre_stddev_samp_fields',
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
                name: 'Chinook_Genre_sum_fields',
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
                name: 'Chinook_Genre_var_pop_fields',
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
                name: 'Chinook_Genre_var_samp_fields',
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
          name: 'Chinook_Genre_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'GenreId',
              description: '',
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
          name: 'Chinook_Genre_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.Genre". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'GenreId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
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
                    name: 'Chinook_Genre_bool_exp',
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
                name: 'Chinook_Genre_bool_exp',
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
                    name: 'Chinook_Genre_bool_exp',
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
          name: 'Chinook_Genre_insert_input',
          description:
            'input type for inserting data into table "Chinook.Genre"',
          fields: null,
          inputFields: [
            {
              name: 'GenreId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: '',
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
          name: 'Chinook_Genre_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'GenreId',
              description: '',
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
              name: 'Name',
              description: '',
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
          name: 'Chinook_Genre_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'GenreId',
              description: '',
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
              name: 'Name',
              description: '',
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
          name: 'Chinook_Genre_mutation_response',
          description: 'response of any mutation on the table "Chinook.Genre"',
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
                      name: 'Chinook_Genre',
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
          name: 'Chinook_Genre_order_by',
          description:
            'Ordering options when selecting data from "Chinook.Genre".',
          fields: null,
          inputFields: [
            {
              name: 'GenreId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_Genre_pk_columns_input',
          description: 'primary key columns input for table: Chinook.Genre',
          fields: null,
          inputFields: [
            {
              name: 'GenreId',
              description: '',
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
          name: 'Chinook_Genre_select_column',
          description: 'select columns of table "Chinook.Genre"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'GenreId',
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
          kind: 'INPUT_OBJECT',
          name: 'Chinook_Genre_set_input',
          description: 'input type for updating data in table "Chinook.Genre"',
          fields: null,
          inputFields: [
            {
              name: 'GenreId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: '',
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
          name: 'Chinook_Genre_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'GenreId',
              description: '',
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
          name: 'Chinook_Genre_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'GenreId',
              description: '',
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
          name: 'Chinook_Genre_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'GenreId',
              description: '',
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
          name: 'Chinook_Genre_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_Genre_set_input',
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
                  name: 'Chinook_Genre_bool_exp',
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
          name: 'Chinook_Genre_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'GenreId',
              description: '',
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
          name: 'Chinook_Genre_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'GenreId',
              description: '',
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
          name: 'Chinook_Invoice',
          description: '',
          fields: [
            {
              name: 'BillingAddress',
              description: '',
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
              name: 'BillingCity',
              description: '',
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
              name: 'BillingCountry',
              description: '',
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
              name: 'BillingPostalCode',
              description: '',
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
              name: 'BillingState',
              description: '',
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
              name: 'CustomerId',
              description: '',
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
              name: 'InvoiceDate',
              description: '',
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'datetime',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'Total',
              description: '',
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Float',
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
          name: 'Chinook_InvoiceLine',
          description: '',
          fields: [
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'InvoiceLineId',
              description: '',
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
              name: 'Quantity',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Float',
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
          name: 'Chinook_InvoiceLine_aggregate',
          description: 'aggregated selection of "Chinook.InvoiceLine"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_InvoiceLine_aggregate_fields',
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
                      name: 'Chinook_InvoiceLine',
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
          name: 'Chinook_InvoiceLine_aggregate_fields',
          description: 'aggregate fields of "Chinook.InvoiceLine"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_InvoiceLine_avg_fields',
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
                    name: 'Chinook_InvoiceLine_select_column',
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
                name: 'Chinook_InvoiceLine_max_fields',
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
                name: 'Chinook_InvoiceLine_min_fields',
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
                name: 'Chinook_InvoiceLine_stddev_pop_fields',
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
                name: 'Chinook_InvoiceLine_stddev_samp_fields',
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
                name: 'Chinook_InvoiceLine_sum_fields',
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
                name: 'Chinook_InvoiceLine_var_pop_fields',
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
                name: 'Chinook_InvoiceLine_var_samp_fields',
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
          name: 'Chinook_InvoiceLine_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'InvoiceLineId',
              description: '',
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
              name: 'Quantity',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_InvoiceLine_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.InvoiceLine". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'InvoiceId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceLineId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Quantity',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Float_mysql8_comparison_exp',
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
                    name: 'Chinook_InvoiceLine_bool_exp',
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
                name: 'Chinook_InvoiceLine_bool_exp',
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
                    name: 'Chinook_InvoiceLine_bool_exp',
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
          name: 'Chinook_InvoiceLine_insert_input',
          description:
            'input type for inserting data into table "Chinook.InvoiceLine"',
          fields: null,
          inputFields: [
            {
              name: 'InvoiceId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceLineId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Quantity',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Chinook_InvoiceLine_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'InvoiceLineId',
              description: '',
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
              name: 'Quantity',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_InvoiceLine_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'InvoiceLineId',
              description: '',
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
              name: 'Quantity',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_InvoiceLine_mutation_response',
          description:
            'response of any mutation on the table "Chinook.InvoiceLine"',
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
                      name: 'Chinook_InvoiceLine',
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
          name: 'Chinook_InvoiceLine_order_by',
          description:
            'Ordering options when selecting data from "Chinook.InvoiceLine".',
          fields: null,
          inputFields: [
            {
              name: 'InvoiceId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceLineId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Quantity',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_InvoiceLine_pk_columns_input',
          description:
            'primary key columns input for table: Chinook.InvoiceLine',
          fields: null,
          inputFields: [
            {
              name: 'InvoiceLineId',
              description: '',
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
          name: 'Chinook_InvoiceLine_select_column',
          description: 'select columns of table "Chinook.InvoiceLine"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'InvoiceId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'InvoiceLineId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Quantity',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'TrackId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'UnitPrice',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Chinook_InvoiceLine_set_input',
          description:
            'input type for updating data in table "Chinook.InvoiceLine"',
          fields: null,
          inputFields: [
            {
              name: 'InvoiceId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceLineId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Quantity',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Chinook_InvoiceLine_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'InvoiceLineId',
              description: '',
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
              name: 'Quantity',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_InvoiceLine_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'InvoiceLineId',
              description: '',
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
              name: 'Quantity',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_InvoiceLine_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'InvoiceLineId',
              description: '',
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
              name: 'Quantity',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_InvoiceLine_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_InvoiceLine_set_input',
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
                  name: 'Chinook_InvoiceLine_bool_exp',
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
          name: 'Chinook_InvoiceLine_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'InvoiceLineId',
              description: '',
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
              name: 'Quantity',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_InvoiceLine_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'InvoiceId',
              description: '',
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
              name: 'InvoiceLineId',
              description: '',
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
              name: 'Quantity',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_Invoice_aggregate',
          description: 'aggregated selection of "Chinook.Invoice"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Invoice_aggregate_fields',
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
                      name: 'Chinook_Invoice',
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
          name: 'Chinook_Invoice_aggregate_fields',
          description: 'aggregate fields of "Chinook.Invoice"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Invoice_avg_fields',
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
                    name: 'Chinook_Invoice_select_column',
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
                name: 'Chinook_Invoice_max_fields',
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
                name: 'Chinook_Invoice_min_fields',
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
                name: 'Chinook_Invoice_stddev_pop_fields',
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
                name: 'Chinook_Invoice_stddev_samp_fields',
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
                name: 'Chinook_Invoice_sum_fields',
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
                name: 'Chinook_Invoice_var_pop_fields',
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
                name: 'Chinook_Invoice_var_samp_fields',
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
          name: 'Chinook_Invoice_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'InvoiceId',
              description: '',
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
              name: 'Total',
              description: '',
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
          name: 'Chinook_Invoice_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.Invoice". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'BillingAddress',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingCity',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingCountry',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingPostalCode',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingState',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceDate',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'datetime_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Total',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Float_mysql8_comparison_exp',
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
                    name: 'Chinook_Invoice_bool_exp',
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
                name: 'Chinook_Invoice_bool_exp',
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
                    name: 'Chinook_Invoice_bool_exp',
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
          name: 'Chinook_Invoice_insert_input',
          description:
            'input type for inserting data into table "Chinook.Invoice"',
          fields: null,
          inputFields: [
            {
              name: 'BillingAddress',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingCity',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingCountry',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingPostalCode',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingState',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceDate',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Total',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Chinook_Invoice_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'BillingAddress',
              description: '',
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
              name: 'BillingCity',
              description: '',
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
              name: 'BillingCountry',
              description: '',
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
              name: 'BillingPostalCode',
              description: '',
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
              name: 'BillingState',
              description: '',
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
              name: 'CustomerId',
              description: '',
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
              name: 'InvoiceId',
              description: '',
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
              name: 'Total',
              description: '',
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
          name: 'Chinook_Invoice_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'BillingAddress',
              description: '',
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
              name: 'BillingCity',
              description: '',
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
              name: 'BillingCountry',
              description: '',
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
              name: 'BillingPostalCode',
              description: '',
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
              name: 'BillingState',
              description: '',
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
              name: 'CustomerId',
              description: '',
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
              name: 'InvoiceId',
              description: '',
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
              name: 'Total',
              description: '',
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
          name: 'Chinook_Invoice_mutation_response',
          description:
            'response of any mutation on the table "Chinook.Invoice"',
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
                      name: 'Chinook_Invoice',
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
          name: 'Chinook_Invoice_order_by',
          description:
            'Ordering options when selecting data from "Chinook.Invoice".',
          fields: null,
          inputFields: [
            {
              name: 'BillingAddress',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingCity',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingCountry',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingPostalCode',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingState',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceDate',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Total',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_Invoice_pk_columns_input',
          description: 'primary key columns input for table: Chinook.Invoice',
          fields: null,
          inputFields: [
            {
              name: 'InvoiceId',
              description: '',
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
          name: 'Chinook_Invoice_select_column',
          description: 'select columns of table "Chinook.Invoice"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'BillingAddress',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'BillingCity',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'BillingCountry',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'BillingPostalCode',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'BillingState',
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
              name: 'InvoiceDate',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'InvoiceId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Total',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Chinook_Invoice_set_input',
          description:
            'input type for updating data in table "Chinook.Invoice"',
          fields: null,
          inputFields: [
            {
              name: 'BillingAddress',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingCity',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingCountry',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingPostalCode',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'BillingState',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceDate',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'InvoiceId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Total',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Chinook_Invoice_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'InvoiceId',
              description: '',
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
              name: 'Total',
              description: '',
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
          name: 'Chinook_Invoice_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'InvoiceId',
              description: '',
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
              name: 'Total',
              description: '',
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
          name: 'Chinook_Invoice_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'InvoiceId',
              description: '',
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
              name: 'Total',
              description: '',
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
          name: 'Chinook_Invoice_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_Invoice_set_input',
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
                  name: 'Chinook_Invoice_bool_exp',
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
          name: 'Chinook_Invoice_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'InvoiceId',
              description: '',
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
              name: 'Total',
              description: '',
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
          name: 'Chinook_Invoice_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'InvoiceId',
              description: '',
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
              name: 'Total',
              description: '',
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
          name: 'Chinook_MediaType',
          description: '',
          fields: [
            {
              name: 'MediaTypeId',
              description: '',
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
              name: 'Name',
              description: '',
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
          name: 'Chinook_MediaType_aggregate',
          description: 'aggregated selection of "Chinook.MediaType"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_MediaType_aggregate_fields',
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
                      name: 'Chinook_MediaType',
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
          name: 'Chinook_MediaType_aggregate_fields',
          description: 'aggregate fields of "Chinook.MediaType"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_MediaType_avg_fields',
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
                    name: 'Chinook_MediaType_select_column',
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
                name: 'Chinook_MediaType_max_fields',
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
                name: 'Chinook_MediaType_min_fields',
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
                name: 'Chinook_MediaType_stddev_pop_fields',
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
                name: 'Chinook_MediaType_stddev_samp_fields',
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
                name: 'Chinook_MediaType_sum_fields',
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
                name: 'Chinook_MediaType_var_pop_fields',
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
                name: 'Chinook_MediaType_var_samp_fields',
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
          name: 'Chinook_MediaType_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'MediaTypeId',
              description: '',
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
          name: 'Chinook_MediaType_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.MediaType". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'MediaTypeId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
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
                    name: 'Chinook_MediaType_bool_exp',
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
                name: 'Chinook_MediaType_bool_exp',
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
                    name: 'Chinook_MediaType_bool_exp',
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
          name: 'Chinook_MediaType_insert_input',
          description:
            'input type for inserting data into table "Chinook.MediaType"',
          fields: null,
          inputFields: [
            {
              name: 'MediaTypeId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: '',
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
          name: 'Chinook_MediaType_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'MediaTypeId',
              description: '',
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
              name: 'Name',
              description: '',
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
          name: 'Chinook_MediaType_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'MediaTypeId',
              description: '',
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
              name: 'Name',
              description: '',
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
          name: 'Chinook_MediaType_mutation_response',
          description:
            'response of any mutation on the table "Chinook.MediaType"',
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
                      name: 'Chinook_MediaType',
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
          name: 'Chinook_MediaType_order_by',
          description:
            'Ordering options when selecting data from "Chinook.MediaType".',
          fields: null,
          inputFields: [
            {
              name: 'MediaTypeId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_MediaType_pk_columns_input',
          description: 'primary key columns input for table: Chinook.MediaType',
          fields: null,
          inputFields: [
            {
              name: 'MediaTypeId',
              description: '',
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
          name: 'Chinook_MediaType_select_column',
          description: 'select columns of table "Chinook.MediaType"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'MediaTypeId',
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
          kind: 'INPUT_OBJECT',
          name: 'Chinook_MediaType_set_input',
          description:
            'input type for updating data in table "Chinook.MediaType"',
          fields: null,
          inputFields: [
            {
              name: 'MediaTypeId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: '',
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
          name: 'Chinook_MediaType_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'MediaTypeId',
              description: '',
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
          name: 'Chinook_MediaType_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'MediaTypeId',
              description: '',
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
          name: 'Chinook_MediaType_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'MediaTypeId',
              description: '',
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
          name: 'Chinook_MediaType_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_MediaType_set_input',
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
                  name: 'Chinook_MediaType_bool_exp',
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
          name: 'Chinook_MediaType_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'MediaTypeId',
              description: '',
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
          name: 'Chinook_MediaType_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'MediaTypeId',
              description: '',
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
          name: 'Chinook_Playlist',
          description: '',
          fields: [
            {
              name: 'Name',
              description: '',
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
              name: 'PlaylistId',
              description: '',
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
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'Chinook_PlaylistTrack',
          description: '',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'Chinook_PlaylistTrack_aggregate',
          description: 'aggregated selection of "Chinook.PlaylistTrack"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_PlaylistTrack_aggregate_fields',
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
                      name: 'Chinook_PlaylistTrack',
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
          name: 'Chinook_PlaylistTrack_aggregate_fields',
          description: 'aggregate fields of "Chinook.PlaylistTrack"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_PlaylistTrack_avg_fields',
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
                    name: 'Chinook_PlaylistTrack_select_column',
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
                name: 'Chinook_PlaylistTrack_max_fields',
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
                name: 'Chinook_PlaylistTrack_min_fields',
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
                name: 'Chinook_PlaylistTrack_stddev_pop_fields',
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
                name: 'Chinook_PlaylistTrack_stddev_samp_fields',
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
                name: 'Chinook_PlaylistTrack_sum_fields',
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
                name: 'Chinook_PlaylistTrack_var_pop_fields',
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
                name: 'Chinook_PlaylistTrack_var_samp_fields',
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
          name: 'Chinook_PlaylistTrack_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          name: 'Chinook_PlaylistTrack_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.PlaylistTrack". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'PlaylistId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
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
                    name: 'Chinook_PlaylistTrack_bool_exp',
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
                name: 'Chinook_PlaylistTrack_bool_exp',
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
                    name: 'Chinook_PlaylistTrack_bool_exp',
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
          name: 'Chinook_PlaylistTrack_insert_input',
          description:
            'input type for inserting data into table "Chinook.PlaylistTrack"',
          fields: null,
          inputFields: [
            {
              name: 'PlaylistId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_PlaylistTrack_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_PlaylistTrack_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_PlaylistTrack_mutation_response',
          description:
            'response of any mutation on the table "Chinook.PlaylistTrack"',
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
                      name: 'Chinook_PlaylistTrack',
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
          name: 'Chinook_PlaylistTrack_order_by',
          description:
            'Ordering options when selecting data from "Chinook.PlaylistTrack".',
          fields: null,
          inputFields: [
            {
              name: 'PlaylistId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_PlaylistTrack_pk_columns_input',
          description:
            'primary key columns input for table: Chinook.PlaylistTrack',
          fields: null,
          inputFields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          name: 'Chinook_PlaylistTrack_select_column',
          description: 'select columns of table "Chinook.PlaylistTrack"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'PlaylistId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'TrackId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Chinook_PlaylistTrack_set_input',
          description:
            'input type for updating data in table "Chinook.PlaylistTrack"',
          fields: null,
          inputFields: [
            {
              name: 'PlaylistId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_PlaylistTrack_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          name: 'Chinook_PlaylistTrack_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          name: 'Chinook_PlaylistTrack_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          name: 'Chinook_PlaylistTrack_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_PlaylistTrack_set_input',
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
                  name: 'Chinook_PlaylistTrack_bool_exp',
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
          name: 'Chinook_PlaylistTrack_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          name: 'Chinook_PlaylistTrack_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
              name: 'TrackId',
              description: '',
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
          name: 'Chinook_Playlist_aggregate',
          description: 'aggregated selection of "Chinook.Playlist"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Playlist_aggregate_fields',
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
                      name: 'Chinook_Playlist',
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
          name: 'Chinook_Playlist_aggregate_fields',
          description: 'aggregate fields of "Chinook.Playlist"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Playlist_avg_fields',
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
                    name: 'Chinook_Playlist_select_column',
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
                name: 'Chinook_Playlist_max_fields',
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
                name: 'Chinook_Playlist_min_fields',
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
                name: 'Chinook_Playlist_stddev_pop_fields',
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
                name: 'Chinook_Playlist_stddev_samp_fields',
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
                name: 'Chinook_Playlist_sum_fields',
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
                name: 'Chinook_Playlist_var_pop_fields',
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
                name: 'Chinook_Playlist_var_samp_fields',
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
          name: 'Chinook_Playlist_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
          name: 'Chinook_Playlist_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.Playlist". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PlaylistId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
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
                    name: 'Chinook_Playlist_bool_exp',
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
                name: 'Chinook_Playlist_bool_exp',
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
                    name: 'Chinook_Playlist_bool_exp',
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
          name: 'Chinook_Playlist_insert_input',
          description:
            'input type for inserting data into table "Chinook.Playlist"',
          fields: null,
          inputFields: [
            {
              name: 'Name',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PlaylistId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_Playlist_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'Name',
              description: '',
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
              name: 'PlaylistId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_Playlist_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'Name',
              description: '',
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
              name: 'PlaylistId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_Playlist_mutation_response',
          description:
            'response of any mutation on the table "Chinook.Playlist"',
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
                      name: 'Chinook_Playlist',
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
          name: 'Chinook_Playlist_order_by',
          description:
            'Ordering options when selecting data from "Chinook.Playlist".',
          fields: null,
          inputFields: [
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PlaylistId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_Playlist_pk_columns_input',
          description: 'primary key columns input for table: Chinook.Playlist',
          fields: null,
          inputFields: [
            {
              name: 'PlaylistId',
              description: '',
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
          name: 'Chinook_Playlist_select_column',
          description: 'select columns of table "Chinook.Playlist"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'Name',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'PlaylistId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Chinook_Playlist_set_input',
          description:
            'input type for updating data in table "Chinook.Playlist"',
          fields: null,
          inputFields: [
            {
              name: 'Name',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PlaylistId',
              description: '',
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
          kind: 'OBJECT',
          name: 'Chinook_Playlist_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
          name: 'Chinook_Playlist_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
          name: 'Chinook_Playlist_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
          name: 'Chinook_Playlist_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_Playlist_set_input',
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
                  name: 'Chinook_Playlist_bool_exp',
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
          name: 'Chinook_Playlist_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
          name: 'Chinook_Playlist_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'PlaylistId',
              description: '',
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
          name: 'Chinook_Track',
          description: '',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'Bytes',
              description: '',
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
              name: 'Composer',
              description: '',
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
              name: 'GenreId',
              description: '',
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
              name: 'MediaTypeId',
              description: '',
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
              name: 'Milliseconds',
              description: '',
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
              name: 'Name',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
              args: [],
              type: {
                kind: 'NON_NULL',
                name: null,
                ofType: {
                  kind: 'SCALAR',
                  name: 'Float',
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
          name: 'Chinook_Track_aggregate',
          description: 'aggregated selection of "Chinook.Track"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Track_aggregate_fields',
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
                      name: 'Chinook_Track',
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
          name: 'Chinook_Track_aggregate_fields',
          description: 'aggregate fields of "Chinook.Track"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Track_avg_fields',
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
                    name: 'Chinook_Track_select_column',
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
                name: 'Chinook_Track_max_fields',
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
                name: 'Chinook_Track_min_fields',
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
                name: 'Chinook_Track_stddev_pop_fields',
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
                name: 'Chinook_Track_stddev_samp_fields',
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
                name: 'Chinook_Track_sum_fields',
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
                name: 'Chinook_Track_var_pop_fields',
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
                name: 'Chinook_Track_var_samp_fields',
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
          name: 'Chinook_Track_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'Bytes',
              description: '',
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
              name: 'GenreId',
              description: '',
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
              name: 'MediaTypeId',
              description: '',
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
              name: 'Milliseconds',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_Track_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Chinook.Track". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Bytes',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Composer',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'GenreId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'MediaTypeId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Milliseconds',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Float_mysql8_comparison_exp',
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
                    name: 'Chinook_Track_bool_exp',
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
                name: 'Chinook_Track_bool_exp',
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
                    name: 'Chinook_Track_bool_exp',
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
          name: 'Chinook_Track_insert_input',
          description:
            'input type for inserting data into table "Chinook.Track"',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Bytes',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Composer',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'GenreId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'MediaTypeId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Milliseconds',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Chinook_Track_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'Bytes',
              description: '',
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
              name: 'Composer',
              description: '',
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
              name: 'GenreId',
              description: '',
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
              name: 'MediaTypeId',
              description: '',
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
              name: 'Milliseconds',
              description: '',
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
              name: 'Name',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_Track_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'Bytes',
              description: '',
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
              name: 'Composer',
              description: '',
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
              name: 'GenreId',
              description: '',
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
              name: 'MediaTypeId',
              description: '',
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
              name: 'Milliseconds',
              description: '',
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
              name: 'Name',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_Track_mutation_response',
          description: 'response of any mutation on the table "Chinook.Track"',
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
                      name: 'Chinook_Track',
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
          name: 'Chinook_Track_order_by',
          description:
            'Ordering options when selecting data from "Chinook.Track".',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Bytes',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Composer',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'GenreId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'MediaTypeId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Milliseconds',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'mysql8_order_by',
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
          name: 'Chinook_Track_pk_columns_input',
          description: 'primary key columns input for table: Chinook.Track',
          fields: null,
          inputFields: [
            {
              name: 'TrackId',
              description: '',
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
          name: 'Chinook_Track_select_column',
          description: 'select columns of table "Chinook.Track"',
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
              name: 'Bytes',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Composer',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'GenreId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'MediaTypeId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Milliseconds',
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
            {
              name: 'TrackId',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'UnitPrice',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Chinook_Track_set_input',
          description: 'input type for updating data in table "Chinook.Track"',
          fields: null,
          inputFields: [
            {
              name: 'AlbumId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Bytes',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Composer',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'GenreId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'MediaTypeId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Milliseconds',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Name',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'TrackId',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
              description: '',
              type: {
                kind: 'SCALAR',
                name: 'Float',
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
          name: 'Chinook_Track_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'Bytes',
              description: '',
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
              name: 'GenreId',
              description: '',
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
              name: 'MediaTypeId',
              description: '',
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
              name: 'Milliseconds',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_Track_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'Bytes',
              description: '',
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
              name: 'GenreId',
              description: '',
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
              name: 'MediaTypeId',
              description: '',
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
              name: 'Milliseconds',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_Track_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'Bytes',
              description: '',
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
              name: 'GenreId',
              description: '',
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
              name: 'MediaTypeId',
              description: '',
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
              name: 'Milliseconds',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_Track_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_set',
              description:
                'sets the columns of the filtered rows to the given values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Chinook_Track_set_input',
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
                  name: 'Chinook_Track_bool_exp',
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
          name: 'Chinook_Track_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'Bytes',
              description: '',
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
              name: 'GenreId',
              description: '',
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
              name: 'MediaTypeId',
              description: '',
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
              name: 'Milliseconds',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          name: 'Chinook_Track_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'AlbumId',
              description: '',
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
              name: 'Bytes',
              description: '',
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
              name: 'GenreId',
              description: '',
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
              name: 'MediaTypeId',
              description: '',
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
              name: 'Milliseconds',
              description: '',
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
              name: 'TrackId',
              description: '',
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
              name: 'UnitPrice',
              description: '',
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
          description: 'columns and relationships of "Customer"',
          fields: [
            {
              name: 'Address',
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
              name: 'City',
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
              name: 'Company',
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
              name: 'Country',
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
              name: 'CustomerId',
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
              name: 'Email',
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
              name: 'Fax',
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
              name: 'FirstName',
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
              name: 'LastName',
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
              name: 'Phone',
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
              name: 'PostalCode',
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
              name: 'State',
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
              name: 'SupportRepId',
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
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Customer_avg_fields',
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
                        name: 'Customer_select_column',
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
                name: 'Customer_max_fields',
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
                name: 'Customer_min_fields',
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
                name: 'Customer_stddev_fields',
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
                name: 'Customer_stddev_pop_fields',
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
                name: 'Customer_stddev_samp_fields',
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
                name: 'Customer_sum_fields',
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
                name: 'Customer_var_pop_fields',
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
                name: 'Customer_var_samp_fields',
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
                name: 'Customer_variance_fields',
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
          name: 'Customer_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'CustomerId',
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
              name: 'SupportRepId',
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
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_comparison_exp',
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
          kind: 'ENUM',
          name: 'Customer_constraint',
          description: 'unique or primary key constraints on table "Customer"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'PK_Customer',
              description:
                'unique or primary key constraint on columns "CustomerId"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Customer_inc_input',
          description:
            'input type for incrementing numeric columns in table "Customer"',
          fields: null,
          inputFields: [
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
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
          name: 'Customer_insert_input',
          description: 'input type for inserting data into table "Customer"',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
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
          kind: 'OBJECT',
          name: 'Customer_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'Address',
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
              name: 'City',
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
              name: 'Company',
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
              name: 'Country',
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
              name: 'CustomerId',
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
              name: 'Email',
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
              name: 'Fax',
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
              name: 'FirstName',
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
              name: 'LastName',
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
              name: 'Phone',
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
              name: 'PostalCode',
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
              name: 'State',
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
              name: 'SupportRepId',
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
          kind: 'OBJECT',
          name: 'Customer_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'Address',
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
              name: 'City',
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
              name: 'Company',
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
              name: 'Country',
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
              name: 'CustomerId',
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
              name: 'Email',
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
              name: 'Fax',
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
              name: 'FirstName',
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
              name: 'LastName',
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
              name: 'Phone',
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
              name: 'PostalCode',
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
              name: 'State',
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
              name: 'SupportRepId',
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
          kind: 'OBJECT',
          name: 'Customer_mutation_response',
          description: 'response of any mutation on the table "Customer"',
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
          kind: 'INPUT_OBJECT',
          name: 'Customer_on_conflict',
          description: 'on_conflict condition type for table "Customer"',
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
                  name: 'Customer_constraint',
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
                      name: 'Customer_update_column',
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
                name: 'Customer_bool_exp',
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
          name: 'Customer_order_by',
          description: 'Ordering options when selecting data from "Customer".',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
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
          name: 'Customer_pk_columns_input',
          description: 'primary key columns input for table: Customer',
          fields: null,
          inputFields: [
            {
              name: 'CustomerId',
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
          kind: 'INPUT_OBJECT',
          name: 'Customer_set_input',
          description: 'input type for updating data in table "Customer"',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
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
          kind: 'OBJECT',
          name: 'Customer_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'CustomerId',
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
              name: 'SupportRepId',
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
          name: 'Customer_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'CustomerId',
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
              name: 'SupportRepId',
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
          name: 'Customer_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'CustomerId',
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
              name: 'SupportRepId',
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
          name: 'Customer_stream_cursor_input',
          description: 'Streaming cursor of the table "Customer"',
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
                  name: 'Customer_stream_cursor_value_input',
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
          name: 'Customer_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
          fields: null,
          inputFields: [
            {
              name: 'Address',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
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
          kind: 'OBJECT',
          name: 'Customer_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'CustomerId',
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
              name: 'SupportRepId',
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
          name: 'Customer_update_column',
          description: 'update columns of table "Customer"',
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
          kind: 'INPUT_OBJECT',
          name: 'Customer_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Customer_inc_input',
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
                name: 'Customer_set_input',
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
                  name: 'Customer_bool_exp',
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
          name: 'Customer_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'CustomerId',
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
              name: 'SupportRepId',
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
          name: 'Customer_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'CustomerId',
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
              name: 'SupportRepId',
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
          name: 'Customer_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'CustomerId',
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
              name: 'SupportRepId',
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
          name: 'Float_mysql8_comparison_exp',
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
          kind: 'OBJECT',
          name: 'INVOICESBYCOUNTRY',
          description: 'columns and relationships of "INVOICESBYCOUNTRY"',
          fields: [
            {
              name: 'BILLINGCOUNTRY',
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
              name: 'INVOICE_TOTAL_BYCOUNTRY',
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
          kind: 'OBJECT',
          name: 'INVOICESBYCOUNTRY_aggregate',
          description: 'aggregated selection of "INVOICESBYCOUNTRY"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'INVOICESBYCOUNTRY_aggregate_fields',
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
                      name: 'INVOICESBYCOUNTRY',
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
          name: 'INVOICESBYCOUNTRY_aggregate_fields',
          description: 'aggregate fields of "INVOICESBYCOUNTRY"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'INVOICESBYCOUNTRY_avg_fields',
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
                    name: 'INVOICESBYCOUNTRY_select_column',
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
                name: 'INVOICESBYCOUNTRY_max_fields',
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
                name: 'INVOICESBYCOUNTRY_min_fields',
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
                name: 'INVOICESBYCOUNTRY_stddev_pop_fields',
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
                name: 'INVOICESBYCOUNTRY_stddev_samp_fields',
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
                name: 'INVOICESBYCOUNTRY_sum_fields',
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
                name: 'INVOICESBYCOUNTRY_var_pop_fields',
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
                name: 'INVOICESBYCOUNTRY_var_samp_fields',
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
          name: 'INVOICESBYCOUNTRY_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'INVOICE_TOTAL_BYCOUNTRY',
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
          name: 'INVOICESBYCOUNTRY_bool_exp',
          description:
            'Boolean expression to filter rows from the table "INVOICESBYCOUNTRY". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'BILLINGCOUNTRY',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_snowflake_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'INVOICE_TOTAL_BYCOUNTRY',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_snowflake_comparison_exp',
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
                    name: 'INVOICESBYCOUNTRY_bool_exp',
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
                name: 'INVOICESBYCOUNTRY_bool_exp',
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
                    name: 'INVOICESBYCOUNTRY_bool_exp',
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
          name: 'INVOICESBYCOUNTRY_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'BILLINGCOUNTRY',
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
              name: 'INVOICE_TOTAL_BYCOUNTRY',
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
          kind: 'OBJECT',
          name: 'INVOICESBYCOUNTRY_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'BILLINGCOUNTRY',
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
              name: 'INVOICE_TOTAL_BYCOUNTRY',
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
          name: 'INVOICESBYCOUNTRY_order_by',
          description:
            'Ordering options when selecting data from "INVOICESBYCOUNTRY".',
          fields: null,
          inputFields: [
            {
              name: 'BILLINGCOUNTRY',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'snowflake_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'INVOICE_TOTAL_BYCOUNTRY',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'snowflake_order_by',
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
          name: 'INVOICESBYCOUNTRY_select_column',
          description: 'select columns of table "INVOICESBYCOUNTRY"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'BILLINGCOUNTRY',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'INVOICE_TOTAL_BYCOUNTRY',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'INVOICESBYCOUNTRY_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'INVOICE_TOTAL_BYCOUNTRY',
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
          name: 'INVOICESBYCOUNTRY_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'INVOICE_TOTAL_BYCOUNTRY',
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
          name: 'INVOICESBYCOUNTRY_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'INVOICE_TOTAL_BYCOUNTRY',
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
          kind: 'OBJECT',
          name: 'INVOICESBYCOUNTRY_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'INVOICE_TOTAL_BYCOUNTRY',
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
          name: 'INVOICESBYCOUNTRY_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'INVOICE_TOTAL_BYCOUNTRY',
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
          kind: 'INPUT_OBJECT',
          name: 'Int_mysql8_comparison_exp',
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
          name: 'Int_snowflake_comparison_exp',
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
          name: 'SEARCH_ALBUMS_args',
          description: null,
          fields: null,
          inputFields: [
            {
              name: 'INPUT_TEXT',
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
          kind: 'INPUT_OBJECT',
          name: 'String_mysql8_comparison_exp',
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
            {
              name: 'contains',
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
          kind: 'INPUT_OBJECT',
          name: 'String_snowflake_comparison_exp',
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
            {
              name: 'contains',
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
          name: 'datetime',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'datetime_mysql8_comparison_exp',
          description:
            'Boolean expression to compare columns of type "datetime". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'datetime',
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
                    name: 'datetime',
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
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'datetime',
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
                    name: 'datetime',
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
              name: 'delete_Album',
              description: 'delete data from the table: "Album"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Album_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Album_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Album_by_pk',
              description: 'delete single row from the table: "Album"',
              args: [
                {
                  name: 'AlbumId',
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
                name: 'Album',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Album',
              description: 'delete data from the table: "Chinook.Album"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Album_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Album_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Album_by_pk',
              description: 'delete single row from the table: "Chinook.Album"',
              args: [
                {
                  name: 'AlbumId',
                  description: '',
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
                name: 'Chinook_Album',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Artist',
              description: 'delete data from the table: "Chinook.Artist"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Artist_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Artist_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Artist_by_pk',
              description: 'delete single row from the table: "Chinook.Artist"',
              args: [
                {
                  name: 'ArtistId',
                  description: '',
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
                name: 'Chinook_Artist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Customer',
              description: 'delete data from the table: "Chinook.Customer"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Customer_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Customer_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Customer_by_pk',
              description:
                'delete single row from the table: "Chinook.Customer"',
              args: [
                {
                  name: 'CustomerId',
                  description: '',
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
                name: 'Chinook_Customer',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Employee',
              description: 'delete data from the table: "Chinook.Employee"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Employee_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Employee_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Employee_by_pk',
              description:
                'delete single row from the table: "Chinook.Employee"',
              args: [
                {
                  name: 'EmployeeId',
                  description: '',
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
                name: 'Chinook_Employee',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Genre',
              description: 'delete data from the table: "Chinook.Genre"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Genre_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Genre_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Genre_by_pk',
              description: 'delete single row from the table: "Chinook.Genre"',
              args: [
                {
                  name: 'GenreId',
                  description: '',
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
                name: 'Chinook_Genre',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Invoice',
              description: 'delete data from the table: "Chinook.Invoice"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Invoice_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Invoice_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_InvoiceLine',
              description: 'delete data from the table: "Chinook.InvoiceLine"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_InvoiceLine_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_InvoiceLine_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_InvoiceLine_by_pk',
              description:
                'delete single row from the table: "Chinook.InvoiceLine"',
              args: [
                {
                  name: 'InvoiceLineId',
                  description: '',
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
                name: 'Chinook_InvoiceLine',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Invoice_by_pk',
              description:
                'delete single row from the table: "Chinook.Invoice"',
              args: [
                {
                  name: 'InvoiceId',
                  description: '',
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
                name: 'Chinook_Invoice',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_MediaType',
              description: 'delete data from the table: "Chinook.MediaType"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_MediaType_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_MediaType_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_MediaType_by_pk',
              description:
                'delete single row from the table: "Chinook.MediaType"',
              args: [
                {
                  name: 'MediaTypeId',
                  description: '',
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
                name: 'Chinook_MediaType',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Playlist',
              description: 'delete data from the table: "Chinook.Playlist"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Playlist_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Playlist_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_PlaylistTrack',
              description:
                'delete data from the table: "Chinook.PlaylistTrack"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_PlaylistTrack_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_PlaylistTrack_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_PlaylistTrack_by_pk',
              description:
                'delete single row from the table: "Chinook.PlaylistTrack"',
              args: [
                {
                  name: 'PlaylistId',
                  description: '',
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
                  name: 'TrackId',
                  description: '',
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
                name: 'Chinook_PlaylistTrack',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Playlist_by_pk',
              description:
                'delete single row from the table: "Chinook.Playlist"',
              args: [
                {
                  name: 'PlaylistId',
                  description: '',
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
                name: 'Chinook_Playlist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Track',
              description: 'delete data from the table: "Chinook.Track"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Track_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Track_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Chinook_Track_by_pk',
              description: 'delete single row from the table: "Chinook.Track"',
              args: [
                {
                  name: 'TrackId',
                  description: '',
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
                name: 'Chinook_Track',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Customer',
              description: 'delete data from the table: "Customer"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Customer_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Customer_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Customer_by_pk',
              description: 'delete single row from the table: "Customer"',
              args: [
                {
                  name: 'CustomerId',
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
                name: 'Customer',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Album',
              description: 'insert data into the table: "Album"',
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
                          name: 'Album_insert_input',
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
                    name: 'Album_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Album_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Album_one',
              description: 'insert a single row into the table: "Album"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Album_insert_input',
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
                    name: 'Album_on_conflict',
                    ofType: null,
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
              name: 'insert_Chinook_Album',
              description: 'insert data into the table: "Chinook.Album"',
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
                          name: 'Chinook_Album_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Album_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Album_one',
              description:
                'insert a single row into the table: "Chinook.Album"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Album_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Album',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Artist',
              description: 'insert data into the table: "Chinook.Artist"',
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
                          name: 'Chinook_Artist_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Artist_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Artist_one',
              description:
                'insert a single row into the table: "Chinook.Artist"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Artist_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Artist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Customer',
              description: 'insert data into the table: "Chinook.Customer"',
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
                          name: 'Chinook_Customer_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Customer_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Customer_one',
              description:
                'insert a single row into the table: "Chinook.Customer"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Customer_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Customer',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Employee',
              description: 'insert data into the table: "Chinook.Employee"',
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
                          name: 'Chinook_Employee_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Employee_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Employee_one',
              description:
                'insert a single row into the table: "Chinook.Employee"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Employee_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Employee',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Genre',
              description: 'insert data into the table: "Chinook.Genre"',
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
                          name: 'Chinook_Genre_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Genre_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Genre_one',
              description:
                'insert a single row into the table: "Chinook.Genre"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Genre_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Genre',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Invoice',
              description: 'insert data into the table: "Chinook.Invoice"',
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
                          name: 'Chinook_Invoice_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Invoice_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_InvoiceLine',
              description: 'insert data into the table: "Chinook.InvoiceLine"',
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
                          name: 'Chinook_InvoiceLine_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_InvoiceLine_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_InvoiceLine_one',
              description:
                'insert a single row into the table: "Chinook.InvoiceLine"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_InvoiceLine_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_InvoiceLine',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Invoice_one',
              description:
                'insert a single row into the table: "Chinook.Invoice"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Invoice_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Invoice',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_MediaType',
              description: 'insert data into the table: "Chinook.MediaType"',
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
                          name: 'Chinook_MediaType_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_MediaType_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_MediaType_one',
              description:
                'insert a single row into the table: "Chinook.MediaType"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_MediaType_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_MediaType',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Playlist',
              description: 'insert data into the table: "Chinook.Playlist"',
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
                          name: 'Chinook_Playlist_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Playlist_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_PlaylistTrack',
              description:
                'insert data into the table: "Chinook.PlaylistTrack"',
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
                          name: 'Chinook_PlaylistTrack_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_PlaylistTrack_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_PlaylistTrack_one',
              description:
                'insert a single row into the table: "Chinook.PlaylistTrack"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_PlaylistTrack_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_PlaylistTrack',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Playlist_one',
              description:
                'insert a single row into the table: "Chinook.Playlist"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Playlist_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Playlist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Track',
              description: 'insert data into the table: "Chinook.Track"',
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
                          name: 'Chinook_Track_insert_input',
                          ofType: null,
                        },
                      },
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Track_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Chinook_Track_one',
              description:
                'insert a single row into the table: "Chinook.Track"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Chinook_Track_insert_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Track',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Customer',
              description: 'insert data into the table: "Customer"',
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
                          name: 'Customer_insert_input',
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
                    name: 'Customer_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Customer_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Customer_one',
              description: 'insert a single row into the table: "Customer"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Customer_insert_input',
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
                    name: 'Customer_on_conflict',
                    ofType: null,
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
              name: 'update_Album',
              description: 'update data of the table: "Album"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Album_inc_input',
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
                    name: 'Album_set_input',
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
                      name: 'Album_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Album_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Album_by_pk',
              description: 'update single row of the table: "Album"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Album_inc_input',
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
                    name: 'Album_set_input',
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
                      name: 'Album_pk_columns_input',
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
              name: 'update_Album_many',
              description: 'update multiples rows of table: "Album"',
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
                          name: 'Album_updates',
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
                  name: 'Album_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Album',
              description: 'update data of the table: "Chinook.Album"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Album_set_input',
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
                      name: 'Chinook_Album_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Album_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Album_by_pk',
              description: 'update single row of the table: "Chinook.Album"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Album_set_input',
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
                      name: 'Chinook_Album_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Album',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Album_many',
              description: 'update multiples rows of table: "Chinook.Album"',
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
                          name: 'Chinook_Album_updates',
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
                  name: 'Chinook_Album_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Artist',
              description: 'update data of the table: "Chinook.Artist"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Artist_set_input',
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
                      name: 'Chinook_Artist_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Artist_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Artist_by_pk',
              description: 'update single row of the table: "Chinook.Artist"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Artist_set_input',
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
                      name: 'Chinook_Artist_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Artist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Artist_many',
              description: 'update multiples rows of table: "Chinook.Artist"',
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
                          name: 'Chinook_Artist_updates',
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
                  name: 'Chinook_Artist_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Customer',
              description: 'update data of the table: "Chinook.Customer"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Customer_set_input',
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
                      name: 'Chinook_Customer_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Customer_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Customer_by_pk',
              description: 'update single row of the table: "Chinook.Customer"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Customer_set_input',
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
                      name: 'Chinook_Customer_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Customer',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Customer_many',
              description: 'update multiples rows of table: "Chinook.Customer"',
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
                          name: 'Chinook_Customer_updates',
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
                  name: 'Chinook_Customer_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Employee',
              description: 'update data of the table: "Chinook.Employee"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Employee_set_input',
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
                      name: 'Chinook_Employee_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Employee_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Employee_by_pk',
              description: 'update single row of the table: "Chinook.Employee"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Employee_set_input',
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
                      name: 'Chinook_Employee_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Employee',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Employee_many',
              description: 'update multiples rows of table: "Chinook.Employee"',
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
                          name: 'Chinook_Employee_updates',
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
                  name: 'Chinook_Employee_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Genre',
              description: 'update data of the table: "Chinook.Genre"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Genre_set_input',
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
                      name: 'Chinook_Genre_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Genre_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Genre_by_pk',
              description: 'update single row of the table: "Chinook.Genre"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Genre_set_input',
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
                      name: 'Chinook_Genre_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Genre',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Genre_many',
              description: 'update multiples rows of table: "Chinook.Genre"',
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
                          name: 'Chinook_Genre_updates',
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
                  name: 'Chinook_Genre_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Invoice',
              description: 'update data of the table: "Chinook.Invoice"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Invoice_set_input',
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
                      name: 'Chinook_Invoice_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Invoice_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_InvoiceLine',
              description: 'update data of the table: "Chinook.InvoiceLine"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_InvoiceLine_set_input',
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
                      name: 'Chinook_InvoiceLine_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_InvoiceLine_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_InvoiceLine_by_pk',
              description:
                'update single row of the table: "Chinook.InvoiceLine"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_InvoiceLine_set_input',
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
                      name: 'Chinook_InvoiceLine_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_InvoiceLine',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_InvoiceLine_many',
              description:
                'update multiples rows of table: "Chinook.InvoiceLine"',
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
                          name: 'Chinook_InvoiceLine_updates',
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
                  name: 'Chinook_InvoiceLine_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Invoice_by_pk',
              description: 'update single row of the table: "Chinook.Invoice"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Invoice_set_input',
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
                      name: 'Chinook_Invoice_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Invoice',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Invoice_many',
              description: 'update multiples rows of table: "Chinook.Invoice"',
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
                          name: 'Chinook_Invoice_updates',
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
                  name: 'Chinook_Invoice_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_MediaType',
              description: 'update data of the table: "Chinook.MediaType"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_MediaType_set_input',
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
                      name: 'Chinook_MediaType_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_MediaType_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_MediaType_by_pk',
              description:
                'update single row of the table: "Chinook.MediaType"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_MediaType_set_input',
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
                      name: 'Chinook_MediaType_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_MediaType',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_MediaType_many',
              description:
                'update multiples rows of table: "Chinook.MediaType"',
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
                          name: 'Chinook_MediaType_updates',
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
                  name: 'Chinook_MediaType_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Playlist',
              description: 'update data of the table: "Chinook.Playlist"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Playlist_set_input',
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
                      name: 'Chinook_Playlist_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Playlist_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_PlaylistTrack',
              description: 'update data of the table: "Chinook.PlaylistTrack"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_PlaylistTrack_set_input',
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
                      name: 'Chinook_PlaylistTrack_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_PlaylistTrack_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_PlaylistTrack_by_pk',
              description:
                'update single row of the table: "Chinook.PlaylistTrack"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_PlaylistTrack_set_input',
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
                      name: 'Chinook_PlaylistTrack_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_PlaylistTrack',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_PlaylistTrack_many',
              description:
                'update multiples rows of table: "Chinook.PlaylistTrack"',
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
                          name: 'Chinook_PlaylistTrack_updates',
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
                  name: 'Chinook_PlaylistTrack_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Playlist_by_pk',
              description: 'update single row of the table: "Chinook.Playlist"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Playlist_set_input',
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
                      name: 'Chinook_Playlist_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Playlist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Playlist_many',
              description: 'update multiples rows of table: "Chinook.Playlist"',
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
                          name: 'Chinook_Playlist_updates',
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
                  name: 'Chinook_Playlist_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Track',
              description: 'update data of the table: "Chinook.Track"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Track_set_input',
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
                      name: 'Chinook_Track_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Track_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Track_by_pk',
              description: 'update single row of the table: "Chinook.Track"',
              args: [
                {
                  name: '_set',
                  description:
                    'sets the columns of the filtered rows to the given values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Chinook_Track_set_input',
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
                      name: 'Chinook_Track_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Chinook_Track',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Chinook_Track_many',
              description: 'update multiples rows of table: "Chinook.Track"',
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
                          name: 'Chinook_Track_updates',
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
                  name: 'Chinook_Track_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Customer',
              description: 'update data of the table: "Customer"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Customer_inc_input',
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
                    name: 'Customer_set_input',
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
                      name: 'Customer_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Customer_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Customer_by_pk',
              description: 'update single row of the table: "Customer"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Customer_inc_input',
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
                    name: 'Customer_set_input',
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
                      name: 'Customer_pk_columns_input',
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
              name: 'update_Customer_many',
              description: 'update multiples rows of table: "Customer"',
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
                          name: 'Customer_updates',
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
                  name: 'Customer_mutation_response',
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
          name: 'mysql8_order_by',
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
              name: 'ALBUM',
              description: 'fetch data from the table: "ALBUM"',
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
                        name: 'ALBUM_order_by',
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
                    name: 'ALBUM_bool_exp',
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
                      name: 'ALBUM',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ALBUM_aggregate',
              description: 'fetch aggregated fields from the table: "ALBUM"',
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
                        name: 'ALBUM_order_by',
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
                    name: 'ALBUM_bool_exp',
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
                  name: 'ALBUM_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ALBUM_by_pk',
              description:
                'fetch data from the table: "ALBUM" using primary key columns',
              args: [
                {
                  name: 'ALBUMID',
                  description: null,
                  type: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'ALBUM',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Album',
              description: 'fetch data from the table: "Album"',
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
                        name: 'Album_select_column',
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
                        name: 'Album_select_column',
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
                      name: 'Int',
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
              name: 'Chinook_Album',
              description: 'fetch data from the table: "Chinook.Album"',
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
                        name: 'Chinook_Album_order_by',
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
                    name: 'Chinook_Album_bool_exp',
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
                      name: 'Chinook_Album',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Album_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Album"',
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
                        name: 'Chinook_Album_order_by',
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
                    name: 'Chinook_Album_bool_exp',
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
                  name: 'Chinook_Album_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Album_by_pk',
              description:
                'fetch data from the table: "Chinook.Album" using primary key columns',
              args: [
                {
                  name: 'AlbumId',
                  description: '',
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
                name: 'Chinook_Album',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Artist',
              description: 'fetch data from the table: "Chinook.Artist"',
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
                        name: 'Chinook_Artist_order_by',
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
                    name: 'Chinook_Artist_bool_exp',
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
                      name: 'Chinook_Artist',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Artist_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Artist"',
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
                        name: 'Chinook_Artist_order_by',
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
                    name: 'Chinook_Artist_bool_exp',
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
                  name: 'Chinook_Artist_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Artist_by_pk',
              description:
                'fetch data from the table: "Chinook.Artist" using primary key columns',
              args: [
                {
                  name: 'ArtistId',
                  description: '',
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
                name: 'Chinook_Artist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Customer',
              description: 'fetch data from the table: "Chinook.Customer"',
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
                        name: 'Chinook_Customer_order_by',
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
                    name: 'Chinook_Customer_bool_exp',
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
                      name: 'Chinook_Customer',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Customer_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Customer"',
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
                        name: 'Chinook_Customer_order_by',
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
                    name: 'Chinook_Customer_bool_exp',
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
                  name: 'Chinook_Customer_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Customer_by_pk',
              description:
                'fetch data from the table: "Chinook.Customer" using primary key columns',
              args: [
                {
                  name: 'CustomerId',
                  description: '',
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
                name: 'Chinook_Customer',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Employee',
              description: 'fetch data from the table: "Chinook.Employee"',
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
                        name: 'Chinook_Employee_order_by',
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
                    name: 'Chinook_Employee_bool_exp',
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
                      name: 'Chinook_Employee',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Employee_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Employee"',
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
                        name: 'Chinook_Employee_order_by',
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
                    name: 'Chinook_Employee_bool_exp',
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
                  name: 'Chinook_Employee_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Employee_by_pk',
              description:
                'fetch data from the table: "Chinook.Employee" using primary key columns',
              args: [
                {
                  name: 'EmployeeId',
                  description: '',
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
                name: 'Chinook_Employee',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Genre',
              description: 'fetch data from the table: "Chinook.Genre"',
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
                        name: 'Chinook_Genre_order_by',
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
                    name: 'Chinook_Genre_bool_exp',
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
                      name: 'Chinook_Genre',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Genre_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Genre"',
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
                        name: 'Chinook_Genre_order_by',
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
                    name: 'Chinook_Genre_bool_exp',
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
                  name: 'Chinook_Genre_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Genre_by_pk',
              description:
                'fetch data from the table: "Chinook.Genre" using primary key columns',
              args: [
                {
                  name: 'GenreId',
                  description: '',
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
                name: 'Chinook_Genre',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Invoice',
              description: 'fetch data from the table: "Chinook.Invoice"',
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
                        name: 'Chinook_Invoice_order_by',
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
                    name: 'Chinook_Invoice_bool_exp',
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
                      name: 'Chinook_Invoice',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_InvoiceLine',
              description: 'fetch data from the table: "Chinook.InvoiceLine"',
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
                        name: 'Chinook_InvoiceLine_order_by',
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
                    name: 'Chinook_InvoiceLine_bool_exp',
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
                      name: 'Chinook_InvoiceLine',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_InvoiceLine_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.InvoiceLine"',
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
                        name: 'Chinook_InvoiceLine_order_by',
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
                    name: 'Chinook_InvoiceLine_bool_exp',
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
                  name: 'Chinook_InvoiceLine_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_InvoiceLine_by_pk',
              description:
                'fetch data from the table: "Chinook.InvoiceLine" using primary key columns',
              args: [
                {
                  name: 'InvoiceLineId',
                  description: '',
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
                name: 'Chinook_InvoiceLine',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Invoice_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Invoice"',
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
                        name: 'Chinook_Invoice_order_by',
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
                    name: 'Chinook_Invoice_bool_exp',
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
                  name: 'Chinook_Invoice_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Invoice_by_pk',
              description:
                'fetch data from the table: "Chinook.Invoice" using primary key columns',
              args: [
                {
                  name: 'InvoiceId',
                  description: '',
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
                name: 'Chinook_Invoice',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_MediaType',
              description: 'fetch data from the table: "Chinook.MediaType"',
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
                        name: 'Chinook_MediaType_order_by',
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
                    name: 'Chinook_MediaType_bool_exp',
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
                      name: 'Chinook_MediaType',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_MediaType_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.MediaType"',
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
                        name: 'Chinook_MediaType_order_by',
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
                    name: 'Chinook_MediaType_bool_exp',
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
                  name: 'Chinook_MediaType_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_MediaType_by_pk',
              description:
                'fetch data from the table: "Chinook.MediaType" using primary key columns',
              args: [
                {
                  name: 'MediaTypeId',
                  description: '',
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
                name: 'Chinook_MediaType',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Playlist',
              description: 'fetch data from the table: "Chinook.Playlist"',
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
                        name: 'Chinook_Playlist_order_by',
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
                    name: 'Chinook_Playlist_bool_exp',
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
                      name: 'Chinook_Playlist',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_PlaylistTrack',
              description: 'fetch data from the table: "Chinook.PlaylistTrack"',
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
                        name: 'Chinook_PlaylistTrack_order_by',
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
                    name: 'Chinook_PlaylistTrack_bool_exp',
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
                      name: 'Chinook_PlaylistTrack',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_PlaylistTrack_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.PlaylistTrack"',
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
                        name: 'Chinook_PlaylistTrack_order_by',
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
                    name: 'Chinook_PlaylistTrack_bool_exp',
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
                  name: 'Chinook_PlaylistTrack_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_PlaylistTrack_by_pk',
              description:
                'fetch data from the table: "Chinook.PlaylistTrack" using primary key columns',
              args: [
                {
                  name: 'PlaylistId',
                  description: '',
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
                  name: 'TrackId',
                  description: '',
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
                name: 'Chinook_PlaylistTrack',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Playlist_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Playlist"',
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
                        name: 'Chinook_Playlist_order_by',
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
                    name: 'Chinook_Playlist_bool_exp',
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
                  name: 'Chinook_Playlist_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Playlist_by_pk',
              description:
                'fetch data from the table: "Chinook.Playlist" using primary key columns',
              args: [
                {
                  name: 'PlaylistId',
                  description: '',
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
                name: 'Chinook_Playlist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Track',
              description: 'fetch data from the table: "Chinook.Track"',
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
                        name: 'Chinook_Track_order_by',
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
                    name: 'Chinook_Track_bool_exp',
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
                      name: 'Chinook_Track',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Track_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Track"',
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
                        name: 'Chinook_Track_order_by',
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
                    name: 'Chinook_Track_bool_exp',
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
                  name: 'Chinook_Track_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Track_by_pk',
              description:
                'fetch data from the table: "Chinook.Track" using primary key columns',
              args: [
                {
                  name: 'TrackId',
                  description: '',
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
                name: 'Chinook_Track',
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
                        name: 'Customer_select_column',
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
                        name: 'Customer_select_column',
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
                      name: 'Int',
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
              name: 'INVOICESBYCOUNTRY',
              description: 'fetch data from the table: "INVOICESBYCOUNTRY"',
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
                        name: 'INVOICESBYCOUNTRY_order_by',
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
                    name: 'INVOICESBYCOUNTRY_bool_exp',
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
                      name: 'INVOICESBYCOUNTRY',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'INVOICESBYCOUNTRY_aggregate',
              description:
                'fetch aggregated fields from the table: "INVOICESBYCOUNTRY"',
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
                        name: 'INVOICESBYCOUNTRY_order_by',
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
                    name: 'INVOICESBYCOUNTRY_bool_exp',
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
                  name: 'INVOICESBYCOUNTRY_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'SEARCH_ALBUMS',
              description:
                'execute function "SEARCH_ALBUMS_BY_TITLE" which returns "ALBUM"',
              args: [
                {
                  name: 'args',
                  description: 'input parameters for function "SEARCH_ALBUMS"',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'SEARCH_ALBUMS_args',
                      ofType: null,
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
                        name: 'ALBUM_order_by',
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
                    name: 'ALBUM_bool_exp',
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
                      name: 'ALBUM',
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
          kind: 'ENUM',
          name: 'snowflake_order_by',
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
              name: 'ALBUM',
              description: 'fetch data from the table: "ALBUM"',
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
                        name: 'ALBUM_order_by',
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
                    name: 'ALBUM_bool_exp',
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
                      name: 'ALBUM',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ALBUM_aggregate',
              description: 'fetch aggregated fields from the table: "ALBUM"',
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
                        name: 'ALBUM_order_by',
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
                    name: 'ALBUM_bool_exp',
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
                  name: 'ALBUM_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'ALBUM_by_pk',
              description:
                'fetch data from the table: "ALBUM" using primary key columns',
              args: [
                {
                  name: 'ALBUMID',
                  description: null,
                  type: {
                    kind: 'SCALAR',
                    name: 'Int',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'ALBUM',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Album',
              description: 'fetch data from the table: "Album"',
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
                        name: 'Album_select_column',
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
                        name: 'Album_select_column',
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
                      name: 'Int',
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
              name: 'Album_stream',
              description:
                'fetch data from the table in a streaming manner: "Album"',
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
                        name: 'Album_stream_cursor_input',
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
              name: 'Chinook_Album',
              description: 'fetch data from the table: "Chinook.Album"',
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
                        name: 'Chinook_Album_order_by',
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
                    name: 'Chinook_Album_bool_exp',
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
                      name: 'Chinook_Album',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Album_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Album"',
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
                        name: 'Chinook_Album_order_by',
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
                    name: 'Chinook_Album_bool_exp',
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
                  name: 'Chinook_Album_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Album_by_pk',
              description:
                'fetch data from the table: "Chinook.Album" using primary key columns',
              args: [
                {
                  name: 'AlbumId',
                  description: '',
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
                name: 'Chinook_Album',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Artist',
              description: 'fetch data from the table: "Chinook.Artist"',
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
                        name: 'Chinook_Artist_order_by',
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
                    name: 'Chinook_Artist_bool_exp',
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
                      name: 'Chinook_Artist',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Artist_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Artist"',
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
                        name: 'Chinook_Artist_order_by',
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
                    name: 'Chinook_Artist_bool_exp',
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
                  name: 'Chinook_Artist_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Artist_by_pk',
              description:
                'fetch data from the table: "Chinook.Artist" using primary key columns',
              args: [
                {
                  name: 'ArtistId',
                  description: '',
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
                name: 'Chinook_Artist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Customer',
              description: 'fetch data from the table: "Chinook.Customer"',
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
                        name: 'Chinook_Customer_order_by',
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
                    name: 'Chinook_Customer_bool_exp',
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
                      name: 'Chinook_Customer',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Customer_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Customer"',
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
                        name: 'Chinook_Customer_order_by',
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
                    name: 'Chinook_Customer_bool_exp',
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
                  name: 'Chinook_Customer_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Customer_by_pk',
              description:
                'fetch data from the table: "Chinook.Customer" using primary key columns',
              args: [
                {
                  name: 'CustomerId',
                  description: '',
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
                name: 'Chinook_Customer',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Employee',
              description: 'fetch data from the table: "Chinook.Employee"',
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
                        name: 'Chinook_Employee_order_by',
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
                    name: 'Chinook_Employee_bool_exp',
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
                      name: 'Chinook_Employee',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Employee_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Employee"',
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
                        name: 'Chinook_Employee_order_by',
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
                    name: 'Chinook_Employee_bool_exp',
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
                  name: 'Chinook_Employee_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Employee_by_pk',
              description:
                'fetch data from the table: "Chinook.Employee" using primary key columns',
              args: [
                {
                  name: 'EmployeeId',
                  description: '',
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
                name: 'Chinook_Employee',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Genre',
              description: 'fetch data from the table: "Chinook.Genre"',
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
                        name: 'Chinook_Genre_order_by',
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
                    name: 'Chinook_Genre_bool_exp',
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
                      name: 'Chinook_Genre',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Genre_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Genre"',
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
                        name: 'Chinook_Genre_order_by',
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
                    name: 'Chinook_Genre_bool_exp',
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
                  name: 'Chinook_Genre_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Genre_by_pk',
              description:
                'fetch data from the table: "Chinook.Genre" using primary key columns',
              args: [
                {
                  name: 'GenreId',
                  description: '',
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
                name: 'Chinook_Genre',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Invoice',
              description: 'fetch data from the table: "Chinook.Invoice"',
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
                        name: 'Chinook_Invoice_order_by',
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
                    name: 'Chinook_Invoice_bool_exp',
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
                      name: 'Chinook_Invoice',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_InvoiceLine',
              description: 'fetch data from the table: "Chinook.InvoiceLine"',
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
                        name: 'Chinook_InvoiceLine_order_by',
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
                    name: 'Chinook_InvoiceLine_bool_exp',
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
                      name: 'Chinook_InvoiceLine',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_InvoiceLine_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.InvoiceLine"',
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
                        name: 'Chinook_InvoiceLine_order_by',
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
                    name: 'Chinook_InvoiceLine_bool_exp',
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
                  name: 'Chinook_InvoiceLine_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_InvoiceLine_by_pk',
              description:
                'fetch data from the table: "Chinook.InvoiceLine" using primary key columns',
              args: [
                {
                  name: 'InvoiceLineId',
                  description: '',
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
                name: 'Chinook_InvoiceLine',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Invoice_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Invoice"',
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
                        name: 'Chinook_Invoice_order_by',
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
                    name: 'Chinook_Invoice_bool_exp',
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
                  name: 'Chinook_Invoice_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Invoice_by_pk',
              description:
                'fetch data from the table: "Chinook.Invoice" using primary key columns',
              args: [
                {
                  name: 'InvoiceId',
                  description: '',
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
                name: 'Chinook_Invoice',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_MediaType',
              description: 'fetch data from the table: "Chinook.MediaType"',
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
                        name: 'Chinook_MediaType_order_by',
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
                    name: 'Chinook_MediaType_bool_exp',
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
                      name: 'Chinook_MediaType',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_MediaType_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.MediaType"',
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
                        name: 'Chinook_MediaType_order_by',
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
                    name: 'Chinook_MediaType_bool_exp',
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
                  name: 'Chinook_MediaType_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_MediaType_by_pk',
              description:
                'fetch data from the table: "Chinook.MediaType" using primary key columns',
              args: [
                {
                  name: 'MediaTypeId',
                  description: '',
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
                name: 'Chinook_MediaType',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Playlist',
              description: 'fetch data from the table: "Chinook.Playlist"',
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
                        name: 'Chinook_Playlist_order_by',
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
                    name: 'Chinook_Playlist_bool_exp',
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
                      name: 'Chinook_Playlist',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_PlaylistTrack',
              description: 'fetch data from the table: "Chinook.PlaylistTrack"',
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
                        name: 'Chinook_PlaylistTrack_order_by',
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
                    name: 'Chinook_PlaylistTrack_bool_exp',
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
                      name: 'Chinook_PlaylistTrack',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_PlaylistTrack_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.PlaylistTrack"',
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
                        name: 'Chinook_PlaylistTrack_order_by',
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
                    name: 'Chinook_PlaylistTrack_bool_exp',
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
                  name: 'Chinook_PlaylistTrack_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_PlaylistTrack_by_pk',
              description:
                'fetch data from the table: "Chinook.PlaylistTrack" using primary key columns',
              args: [
                {
                  name: 'PlaylistId',
                  description: '',
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
                  name: 'TrackId',
                  description: '',
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
                name: 'Chinook_PlaylistTrack',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Playlist_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Playlist"',
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
                        name: 'Chinook_Playlist_order_by',
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
                    name: 'Chinook_Playlist_bool_exp',
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
                  name: 'Chinook_Playlist_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Playlist_by_pk',
              description:
                'fetch data from the table: "Chinook.Playlist" using primary key columns',
              args: [
                {
                  name: 'PlaylistId',
                  description: '',
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
                name: 'Chinook_Playlist',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Track',
              description: 'fetch data from the table: "Chinook.Track"',
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
                        name: 'Chinook_Track_order_by',
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
                    name: 'Chinook_Track_bool_exp',
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
                      name: 'Chinook_Track',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Track_aggregate',
              description:
                'fetch aggregated fields from the table: "Chinook.Track"',
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
                        name: 'Chinook_Track_order_by',
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
                    name: 'Chinook_Track_bool_exp',
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
                  name: 'Chinook_Track_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Chinook_Track_by_pk',
              description:
                'fetch data from the table: "Chinook.Track" using primary key columns',
              args: [
                {
                  name: 'TrackId',
                  description: '',
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
                name: 'Chinook_Track',
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
                        name: 'Customer_select_column',
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
                        name: 'Customer_select_column',
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
                      name: 'Int',
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
              name: 'Customer_stream',
              description:
                'fetch data from the table in a streaming manner: "Customer"',
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
                        name: 'Customer_stream_cursor_input',
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
              name: 'INVOICESBYCOUNTRY',
              description: 'fetch data from the table: "INVOICESBYCOUNTRY"',
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
                        name: 'INVOICESBYCOUNTRY_order_by',
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
                    name: 'INVOICESBYCOUNTRY_bool_exp',
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
                      name: 'INVOICESBYCOUNTRY',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'INVOICESBYCOUNTRY_aggregate',
              description:
                'fetch aggregated fields from the table: "INVOICESBYCOUNTRY"',
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
                        name: 'INVOICESBYCOUNTRY_order_by',
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
                    name: 'INVOICESBYCOUNTRY_bool_exp',
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
                  name: 'INVOICESBYCOUNTRY_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'SEARCH_ALBUMS',
              description:
                'execute function "SEARCH_ALBUMS_BY_TITLE" which returns "ALBUM"',
              args: [
                {
                  name: 'args',
                  description: 'input parameters for function "SEARCH_ALBUMS"',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'SEARCH_ALBUMS_args',
                      ofType: null,
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
                        name: 'ALBUM_order_by',
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
                    name: 'ALBUM_bool_exp',
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
                      name: 'ALBUM',
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

export const schema = new GraphQLSchema(
  buildClientSchema(
    introspection.data as unknown as IntrospectionQuery
  ).toConfig()
);
