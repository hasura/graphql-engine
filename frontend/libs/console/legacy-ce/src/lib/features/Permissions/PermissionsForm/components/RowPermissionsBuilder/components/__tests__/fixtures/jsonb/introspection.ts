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
          name: 'Author',
          description: 'columns and relationships of "Author"',
          fields: [
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
              name: 'surname',
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
          name: 'Author_aggregate',
          description: 'aggregated selection of "Author"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Author_aggregate_fields',
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
                      name: 'Author',
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
          name: 'Author_aggregate_fields',
          description: 'aggregate fields of "Author"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Author_avg_fields',
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
                        name: 'Author_select_column',
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
                name: 'Author_max_fields',
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
                name: 'Author_min_fields',
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
                name: 'Author_stddev_fields',
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
                name: 'Author_stddev_pop_fields',
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
                name: 'Author_stddev_samp_fields',
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
                name: 'Author_sum_fields',
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
                name: 'Author_var_pop_fields',
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
                name: 'Author_var_samp_fields',
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
                name: 'Author_variance_fields',
                ofType: null,
              },
              isDeprecated: false,
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
          name: 'Author_avg_fields',
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
          name: 'Author_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Author". All fields are combined with a logical \'AND\'.',
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
                    name: 'Author_bool_exp',
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
                name: 'Author_bool_exp',
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
                    name: 'Author_bool_exp',
                    ofType: null,
                  },
                },
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
              name: 'surname',
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
          name: 'Author_constraint',
          description: 'unique or primary key constraints on table "Author"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'Author_pkey',
              description: 'unique or primary key constraint on columns "id"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Author_inc_input',
          description:
            'input type for incrementing numeric columns in table "Author"',
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
          name: 'Author_insert_input',
          description: 'input type for inserting data into table "Author"',
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
              name: 'surname',
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
          name: 'Author_max_fields',
          description: 'aggregate max on columns',
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
            {
              name: 'surname',
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
          name: 'Author_min_fields',
          description: 'aggregate min on columns',
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
            {
              name: 'surname',
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
          name: 'Author_mutation_response',
          description: 'response of any mutation on the table "Author"',
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
                      name: 'Author',
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
          name: 'Author_obj_rel_insert_input',
          description:
            'input type for inserting object relation for remote table "Author"',
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
                  name: 'Author_insert_input',
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
                name: 'Author_on_conflict',
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
          name: 'Author_on_conflict',
          description: 'on_conflict condition type for table "Author"',
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
                  name: 'Author_constraint',
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
                      name: 'Author_update_column',
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
                name: 'Author_bool_exp',
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
          name: 'Author_order_by',
          description: 'Ordering options when selecting data from "Author".',
          fields: null,
          inputFields: [
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
              name: 'surname',
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
          name: 'Author_pk_columns_input',
          description: 'primary key columns input for table: Author',
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
          name: 'Author_select_column',
          description: 'select columns of table "Author"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
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
            {
              name: 'surname',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Author_set_input',
          description: 'input type for updating data in table "Author"',
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
              name: 'surname',
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
          name: 'Author_stddev_fields',
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
          name: 'Author_stddev_pop_fields',
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
          name: 'Author_stddev_samp_fields',
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
          name: 'Author_stream_cursor_input',
          description: 'Streaming cursor of the table "Author"',
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
                  name: 'Author_stream_cursor_value_input',
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
          name: 'Author_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
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
              name: 'surname',
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
          name: 'Author_sum_fields',
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
          name: 'Author_update_column',
          description: 'update columns of table "Author"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
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
            {
              name: 'surname',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Author_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Author_inc_input',
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
                name: 'Author_set_input',
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
                  name: 'Author_bool_exp',
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
          name: 'Author_var_pop_fields',
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
          name: 'Author_var_samp_fields',
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
          name: 'Author_variance_fields',
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
          name: 'Boolean_comparison_exp',
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
          name: 'Cart',
          description: 'columns and relationships of "Cart"',
          fields: [
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
          name: 'Cart_aggregate',
          description: 'aggregated selection of "Cart"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Cart_aggregate_fields',
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
                      name: 'Cart',
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
          name: 'Cart_aggregate_fields',
          description: 'aggregate fields of "Cart"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Cart_avg_fields',
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
                        name: 'Cart_select_column',
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
                name: 'Cart_max_fields',
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
                name: 'Cart_min_fields',
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
                name: 'Cart_stddev_fields',
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
                name: 'Cart_stddev_pop_fields',
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
                name: 'Cart_stddev_samp_fields',
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
                name: 'Cart_sum_fields',
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
                name: 'Cart_var_pop_fields',
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
                name: 'Cart_var_samp_fields',
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
                name: 'Cart_variance_fields',
                ofType: null,
              },
              isDeprecated: false,
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
          name: 'Cart_avg_fields',
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
          name: 'Cart_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Cart". All fields are combined with a logical \'AND\'.',
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
                    name: 'Cart_bool_exp',
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
                name: 'Cart_bool_exp',
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
                    name: 'Cart_bool_exp',
                    ofType: null,
                  },
                },
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
          name: 'Cart_constraint',
          description: 'unique or primary key constraints on table "Cart"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'Cart_pkey',
              description: 'unique or primary key constraint on columns "id"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Cart_inc_input',
          description:
            'input type for incrementing numeric columns in table "Cart"',
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
          name: 'Cart_insert_input',
          description: 'input type for inserting data into table "Cart"',
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
          name: 'Cart_max_fields',
          description: 'aggregate max on columns',
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
          name: 'Cart_min_fields',
          description: 'aggregate min on columns',
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
          name: 'Cart_mutation_response',
          description: 'response of any mutation on the table "Cart"',
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
                      name: 'Cart',
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
          name: 'Cart_obj_rel_insert_input',
          description:
            'input type for inserting object relation for remote table "Cart"',
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
                  name: 'Cart_insert_input',
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
                name: 'Cart_on_conflict',
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
          name: 'Cart_on_conflict',
          description: 'on_conflict condition type for table "Cart"',
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
                  name: 'Cart_constraint',
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
                      name: 'Cart_update_column',
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
                name: 'Cart_bool_exp',
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
          name: 'Cart_order_by',
          description: 'Ordering options when selecting data from "Cart".',
          fields: null,
          inputFields: [
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
          name: 'Cart_pk_columns_input',
          description: 'primary key columns input for table: Cart',
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
          name: 'Cart_select_column',
          description: 'select columns of table "Cart"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
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
          name: 'Cart_set_input',
          description: 'input type for updating data in table "Cart"',
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
          name: 'Cart_stddev_fields',
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
          name: 'Cart_stddev_pop_fields',
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
          name: 'Cart_stddev_samp_fields',
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
          name: 'Cart_stream_cursor_input',
          description: 'Streaming cursor of the table "Cart"',
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
                  name: 'Cart_stream_cursor_value_input',
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
          name: 'Cart_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
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
          name: 'Cart_sum_fields',
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
          name: 'Cart_update_column',
          description: 'update columns of table "Cart"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
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
          name: 'Cart_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Cart_inc_input',
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
                name: 'Cart_set_input',
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
                  name: 'Cart_bool_exp',
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
          name: 'Cart_var_pop_fields',
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
          name: 'Cart_var_samp_fields',
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
          name: 'Cart_variance_fields',
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
          name: 'Chinook_Customer',
          description: '',
          fields: [
            {
              name: 'Address',
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
                name: 'number',
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
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'City',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Company',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Country',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'CustomerId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'number_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Email',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Fax',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'FirstName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'LastName',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Phone',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'PostalCode',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'State',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_mysql8_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'SupportRepId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'number_mysql8_comparison_exp',
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
                name: 'string',
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
                name: 'string',
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
                name: 'string',
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
                name: 'string',
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
                name: 'number',
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
                name: 'string',
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
                name: 'string',
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
                name: 'string',
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
                name: 'string',
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
                name: 'string',
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
                name: 'string',
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
                name: 'string',
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
          name: 'Chinook_Customer_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'Address',
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
              name: 'Email',
              description: '',
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
              name: 'Fax',
              description: '',
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
              description: '',
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
              name: 'LastName',
              description: '',
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
              name: 'Phone',
              description: '',
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
              description: '',
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
              description: '',
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
              description: '',
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
                name: 'number',
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
          name: 'Chinook_Customer_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Chinook_Customer_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Chinook_Customer_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Chinook_Customer_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'CustomerId',
              description: '',
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
              name: 'SupportRepId',
              description: '',
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
          name: 'Datetime',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Datetime_BigQuery_comparison_exp',
          description:
            'Boolean expression to compare columns of type "Datetime". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Datetime',
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
                    name: 'Datetime',
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
                name: 'Datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Datetime',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Datetime',
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
                    name: 'Datetime',
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
          name: 'Record',
          description: 'columns and relationships of "Record"',
          fields: [
            {
              name: 'Author',
              description: 'An object relationship',
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Author',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'author_id',
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
              name: 'title',
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
          name: 'Record_aggregate',
          description: 'aggregated selection of "Record"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Record_aggregate_fields',
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
                      name: 'Record',
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
          name: 'Record_aggregate_fields',
          description: 'aggregate fields of "Record"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Record_avg_fields',
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
                        name: 'Record_select_column',
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
                name: 'Record_max_fields',
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
                name: 'Record_min_fields',
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
                name: 'Record_stddev_fields',
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
                name: 'Record_stddev_pop_fields',
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
                name: 'Record_stddev_samp_fields',
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
                name: 'Record_sum_fields',
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
                name: 'Record_var_pop_fields',
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
                name: 'Record_var_samp_fields',
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
                name: 'Record_variance_fields',
                ofType: null,
              },
              isDeprecated: false,
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
          name: 'Record_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'Record_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Record". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'Author',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Author_bool_exp',
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
                    name: 'Record_bool_exp',
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
                name: 'Record_bool_exp',
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
                    name: 'Record_bool_exp',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: 'author_id',
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
              name: 'title',
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
          name: 'Record_constraint',
          description: 'unique or primary key constraints on table "Record"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'Record_pkey',
              description: 'unique or primary key constraint on columns "id"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Record_inc_input',
          description:
            'input type for incrementing numeric columns in table "Record"',
          fields: null,
          inputFields: [
            {
              name: 'author_id',
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
          name: 'Record_insert_input',
          description: 'input type for inserting data into table "Record"',
          fields: null,
          inputFields: [
            {
              name: 'Author',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Author_obj_rel_insert_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'author_id',
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
              name: 'title',
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
          name: 'Record_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'author_id',
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
              name: 'title',
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
          name: 'Record_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'author_id',
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
              name: 'title',
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
          name: 'Record_mutation_response',
          description: 'response of any mutation on the table "Record"',
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
                      name: 'Record',
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
          name: 'Record_on_conflict',
          description: 'on_conflict condition type for table "Record"',
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
                  name: 'Record_constraint',
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
                      name: 'Record_update_column',
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
                name: 'Record_bool_exp',
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
          name: 'Record_order_by',
          description: 'Ordering options when selecting data from "Record".',
          fields: null,
          inputFields: [
            {
              name: 'Author',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Author_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'author_id',
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
              name: 'title',
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
          name: 'Record_pk_columns_input',
          description: 'primary key columns input for table: Record',
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
          name: 'Record_select_column',
          description: 'select columns of table "Record"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'author_id',
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
              name: 'title',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Record_set_input',
          description: 'input type for updating data in table "Record"',
          fields: null,
          inputFields: [
            {
              name: 'author_id',
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
              name: 'title',
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
          name: 'Record_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'Record_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'Record_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'Record_stream_cursor_input',
          description: 'Streaming cursor of the table "Record"',
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
                  name: 'Record_stream_cursor_value_input',
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
          name: 'Record_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
          fields: null,
          inputFields: [
            {
              name: 'author_id',
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
              name: 'title',
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
          name: 'Record_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'Record_update_column',
          description: 'update columns of table "Record"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'author_id',
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
              name: 'title',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Record_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Record_inc_input',
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
                name: 'Record_set_input',
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
                  name: 'Record_bool_exp',
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
          name: 'Record_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'Record_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'Record_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'Stuff',
          description: 'columns and relationships of "Stuff"',
          fields: [
            {
              name: 'Cart',
              description: 'An object relationship',
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Cart',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'box',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'box',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'cart_id',
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
              name: 'enabled',
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
              name: 'jason',
              description: null,
              args: [
                {
                  name: 'path',
                  description: 'JSON select path',
                  type: {
                    kind: 'SCALAR',
                    name: 'String',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
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
          name: 'Stuff_aggregate',
          description: 'aggregated selection of "Stuff"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Stuff_aggregate_fields',
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
                      name: 'Stuff',
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
          name: 'Stuff_aggregate_fields',
          description: 'aggregate fields of "Stuff"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Stuff_avg_fields',
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
                        name: 'Stuff_select_column',
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
                name: 'Stuff_max_fields',
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
                name: 'Stuff_min_fields',
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
                name: 'Stuff_stddev_fields',
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
                name: 'Stuff_stddev_pop_fields',
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
                name: 'Stuff_stddev_samp_fields',
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
                name: 'Stuff_sum_fields',
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
                name: 'Stuff_var_pop_fields',
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
                name: 'Stuff_var_samp_fields',
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
                name: 'Stuff_variance_fields',
                ofType: null,
              },
              isDeprecated: false,
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
          name: 'Stuff_append_input',
          description:
            'append existing jsonb value of filtered columns with new jsonb value',
          fields: null,
          inputFields: [
            {
              name: 'jason',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
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
          name: 'Stuff_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Stuff". All fields are combined with a logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: 'Cart',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Cart_bool_exp',
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
                    name: 'Stuff_bool_exp',
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
                name: 'Stuff_bool_exp',
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
                    name: 'Stuff_bool_exp',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: 'box',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'box_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'cart_id',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'enabled',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Boolean_comparison_exp',
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
              name: 'jason',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'jsonb_comparison_exp',
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
          name: 'Stuff_constraint',
          description: 'unique or primary key constraints on table "Stuff"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'Stuff_pkey',
              description: 'unique or primary key constraint on columns "id"',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'Stuff_delete_at_path_input',
          description:
            'delete the field or element with specified path (for JSON arrays, negative integers count from the end)',
          fields: null,
          inputFields: [
            {
              name: 'jason',
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
          name: 'Stuff_delete_elem_input',
          description:
            'delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array',
          fields: null,
          inputFields: [
            {
              name: 'jason',
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
          name: 'Stuff_delete_key_input',
          description:
            'delete key/value pair or string element. key/value pairs are matched based on their key value',
          fields: null,
          inputFields: [
            {
              name: 'jason',
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
          name: 'Stuff_inc_input',
          description:
            'input type for incrementing numeric columns in table "Stuff"',
          fields: null,
          inputFields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_insert_input',
          description: 'input type for inserting data into table "Stuff"',
          fields: null,
          inputFields: [
            {
              name: 'Cart',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Cart_obj_rel_insert_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'box',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'box',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'cart_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'enabled',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
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
              name: 'jason',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
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
          name: 'Stuff_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_mutation_response',
          description: 'response of any mutation on the table "Stuff"',
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
                      name: 'Stuff',
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
          name: 'Stuff_on_conflict',
          description: 'on_conflict condition type for table "Stuff"',
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
                  name: 'Stuff_constraint',
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
                      name: 'Stuff_update_column',
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
                name: 'Stuff_bool_exp',
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
          name: 'Stuff_order_by',
          description: 'Ordering options when selecting data from "Stuff".',
          fields: null,
          inputFields: [
            {
              name: 'Cart',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Cart_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'box',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'cart_id',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'enabled',
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
              name: 'jason',
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
          name: 'Stuff_pk_columns_input',
          description: 'primary key columns input for table: Stuff',
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
          kind: 'INPUT_OBJECT',
          name: 'Stuff_prepend_input',
          description:
            'prepend existing jsonb value of filtered columns with new jsonb value',
          fields: null,
          inputFields: [
            {
              name: 'jason',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
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
          name: 'Stuff_select_column',
          description: 'select columns of table "Stuff"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'box',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'cart_id',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'enabled',
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
              name: 'jason',
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
          name: 'Stuff_set_input',
          description: 'input type for updating data in table "Stuff"',
          fields: null,
          inputFields: [
            {
              name: 'box',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'box',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'cart_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'enabled',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
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
              name: 'jason',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
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
          name: 'Stuff_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_stream_cursor_input',
          description: 'Streaming cursor of the table "Stuff"',
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
                  name: 'Stuff_stream_cursor_value_input',
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
          name: 'Stuff_stream_cursor_value_input',
          description:
            'Initial value of the column from where the streaming should start',
          fields: null,
          inputFields: [
            {
              name: 'box',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'box',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'cart_id',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Int',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'enabled',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'Boolean',
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
              name: 'jason',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
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
          name: 'Stuff_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_update_column',
          description: 'update columns of table "Stuff"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'box',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'cart_id',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'enabled',
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
              name: 'jason',
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
          name: 'Stuff_updates',
          description: null,
          fields: null,
          inputFields: [
            {
              name: '_append',
              description:
                'append existing jsonb value of filtered columns with new jsonb value',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Stuff_append_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_delete_at_path',
              description:
                'delete the field or element with specified path (for JSON arrays, negative integers count from the end)',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Stuff_delete_at_path_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_delete_elem',
              description:
                'delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Stuff_delete_elem_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_delete_key',
              description:
                'delete key/value pair or string element. key/value pairs are matched based on their key value',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Stuff_delete_key_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_inc',
              description:
                'increments the numeric columns with given value of the filtered values',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Stuff_inc_input',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_prepend',
              description:
                'prepend existing jsonb value of filtered columns with new jsonb value',
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Stuff_prepend_input',
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
                name: 'Stuff_set_input',
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
                  name: 'Stuff_bool_exp',
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
          name: 'Stuff_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Stuff_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'cart_id',
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
          name: 'Track',
          description:
            'CREATE TABLE [Track]\n(\n    [TrackId] INTEGER  NOT NULL,\n    [Name] NVARCHAR(200)  NOT NULL,\n    [AlbumId] INTEGER,\n    [MediaTypeId] INTEGER  NOT NULL,\n    [GenreId] INTEGER,\n    [Composer] NVARCHAR(220),\n    [Milliseconds] INTEGER  NOT NULL,\n    [Bytes] INTEGER,\n    [UnitPrice] NUMERIC(10,2)  NOT NULL,\n    CONSTRAINT [PK_Track] PRIMARY KEY  ([TrackId]),\n    FOREIGN KEY ([AlbumId]) REFERENCES [Album] ([AlbumId]) \n\t\tON DELETE NO ACTION ON UPDATE NO ACTION,\n    FOREIGN KEY ([GenreId]) REFERENCES [Genre] ([GenreId]) \n\t\tON DELETE NO ACTION ON UPDATE NO ACTION,\n    FOREIGN KEY ([MediaTypeId]) REFERENCES [MediaType] ([MediaTypeId]) \n\t\tON DELETE NO ACTION ON UPDATE NO ACTION\n)',
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
              name: 'Bytes',
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
              name: 'Composer',
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
              name: 'GenreId',
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
              name: 'MediaTypeId',
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
              name: 'Milliseconds',
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
              name: 'TrackId',
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
              name: 'UnitPrice',
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
          ],
          inputFields: null,
          interfaces: [],
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'Track_aggregate',
          description: 'aggregated selection of "Track"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'Track_aggregate_fields',
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
                      name: 'Track',
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
          name: 'Track_aggregate_fields',
          description: 'aggregate fields of "Track"',
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
                    name: 'Track_select_column',
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
          name: 'Track_bool_exp',
          description:
            'Boolean expression to filter rows from the table "Track". All fields are combined with a logical \'AND\'.',
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
              name: 'Bytes',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'number_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Composer',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'string_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'GenreId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'number_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'MediaTypeId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'number_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Milliseconds',
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
              name: 'TrackId',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'number_SQLite_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
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
                    name: 'Track_bool_exp',
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
                name: 'Track_bool_exp',
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
                    name: 'Track_bool_exp',
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
          name: 'Track_order_by',
          description: 'Ordering options when selecting data from "Track".',
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
              name: 'Bytes',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Composer',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'GenreId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'MediaTypeId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'Milliseconds',
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
            {
              name: 'TrackId',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'SQLite_order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'UnitPrice',
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
          name: 'Track_select_column',
          description: 'select columns of table "Track"',
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
          name: 'box',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'box_comparison_exp',
          description:
            'Boolean expression to compare columns of type "box". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'box',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'box',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'box',
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
                    name: 'box',
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
                name: 'box',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'box',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'box',
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
                    name: 'box',
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
          name: 'jsonb',
          description: null,
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: null,
          possibleTypes: null,
        },
        {
          kind: 'INPUT_OBJECT',
          name: 'jsonb_cast_exp',
          description: null,
          fields: null,
          inputFields: [
            {
              name: 'String',
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
          kind: 'INPUT_OBJECT',
          name: 'jsonb_comparison_exp',
          description:
            'Boolean expression to compare columns of type "jsonb". All fields are combined with logical \'AND\'.',
          fields: null,
          inputFields: [
            {
              name: '_cast',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'jsonb_cast_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_contained_in',
              description: 'is the column contained in the given json value',
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_contains',
              description:
                'does the column contain the given json value at the top level',
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_eq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gt',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_gte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_has_key',
              description:
                'does the string exist as a top-level key in the column',
              type: {
                kind: 'SCALAR',
                name: 'String',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_has_keys_all',
              description:
                'do all of these strings exist as top-level keys in the column',
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
              name: '_has_keys_any',
              description:
                'do any of these strings exist as top-level keys in the column',
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
                    name: 'jsonb',
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
                name: 'jsonb',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_lte',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: '_neq',
              description: null,
              type: {
                kind: 'SCALAR',
                name: 'jsonb',
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
                    name: 'jsonb',
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
              name: 'delete_Author',
              description: 'delete data from the table: "Author"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Author_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Author_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Author_by_pk',
              description: 'delete single row from the table: "Author"',
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
                name: 'Author',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Cart',
              description: 'delete data from the table: "Cart"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Cart_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Cart_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Cart_by_pk',
              description: 'delete single row from the table: "Cart"',
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
                name: 'Cart',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Record',
              description: 'delete data from the table: "Record"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Record_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Record_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Record_by_pk',
              description: 'delete single row from the table: "Record"',
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
                name: 'Record',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Stuff',
              description: 'delete data from the table: "Stuff"',
              args: [
                {
                  name: 'where',
                  description: 'filter the rows which have to be deleted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Stuff_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Stuff_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'delete_Stuff_by_pk',
              description: 'delete single row from the table: "Stuff"',
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
                name: 'Stuff',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Author',
              description: 'insert data into the table: "Author"',
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
                          name: 'Author_insert_input',
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
                    name: 'Author_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Author_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Author_one',
              description: 'insert a single row into the table: "Author"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Author_insert_input',
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
                    name: 'Author_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Author',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Cart',
              description: 'insert data into the table: "Cart"',
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
                          name: 'Cart_insert_input',
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
                    name: 'Cart_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Cart_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Cart_one',
              description: 'insert a single row into the table: "Cart"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Cart_insert_input',
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
                    name: 'Cart_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Cart',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Record',
              description: 'insert data into the table: "Record"',
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
                          name: 'Record_insert_input',
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
                    name: 'Record_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Record_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Record_one',
              description: 'insert a single row into the table: "Record"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Record_insert_input',
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
                    name: 'Record_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Record',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Stuff',
              description: 'insert data into the table: "Stuff"',
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
                          name: 'Stuff_insert_input',
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
                    name: 'Stuff_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Stuff_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'insert_Stuff_one',
              description: 'insert a single row into the table: "Stuff"',
              args: [
                {
                  name: 'object',
                  description: 'the row to be inserted',
                  type: {
                    kind: 'NON_NULL',
                    name: null,
                    ofType: {
                      kind: 'INPUT_OBJECT',
                      name: 'Stuff_insert_input',
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
                    name: 'Stuff_on_conflict',
                    ofType: null,
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Stuff',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Author',
              description: 'update data of the table: "Author"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Author_inc_input',
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
                    name: 'Author_set_input',
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
                      name: 'Author_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Author_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Author_by_pk',
              description: 'update single row of the table: "Author"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Author_inc_input',
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
                    name: 'Author_set_input',
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
                      name: 'Author_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Author',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Author_many',
              description: 'update multiples rows of table: "Author"',
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
                          name: 'Author_updates',
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
                  name: 'Author_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Cart',
              description: 'update data of the table: "Cart"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Cart_inc_input',
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
                    name: 'Cart_set_input',
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
                      name: 'Cart_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Cart_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Cart_by_pk',
              description: 'update single row of the table: "Cart"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Cart_inc_input',
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
                    name: 'Cart_set_input',
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
                      name: 'Cart_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Cart',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Cart_many',
              description: 'update multiples rows of table: "Cart"',
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
                          name: 'Cart_updates',
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
                  name: 'Cart_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Record',
              description: 'update data of the table: "Record"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Record_inc_input',
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
                    name: 'Record_set_input',
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
                      name: 'Record_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Record_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Record_by_pk',
              description: 'update single row of the table: "Record"',
              args: [
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Record_inc_input',
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
                    name: 'Record_set_input',
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
                      name: 'Record_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Record',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Record_many',
              description: 'update multiples rows of table: "Record"',
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
                          name: 'Record_updates',
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
                  name: 'Record_mutation_response',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Stuff',
              description: 'update data of the table: "Stuff"',
              args: [
                {
                  name: '_append',
                  description:
                    'append existing jsonb value of filtered columns with new jsonb value',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_append_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_delete_at_path',
                  description:
                    'delete the field or element with specified path (for JSON arrays, negative integers count from the end)',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_delete_at_path_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_delete_elem',
                  description:
                    'delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_delete_elem_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_delete_key',
                  description:
                    'delete key/value pair or string element. key/value pairs are matched based on their key value',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_delete_key_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_inc_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_prepend',
                  description:
                    'prepend existing jsonb value of filtered columns with new jsonb value',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_prepend_input',
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
                    name: 'Stuff_set_input',
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
                      name: 'Stuff_bool_exp',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Stuff_mutation_response',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Stuff_by_pk',
              description: 'update single row of the table: "Stuff"',
              args: [
                {
                  name: '_append',
                  description:
                    'append existing jsonb value of filtered columns with new jsonb value',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_append_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_delete_at_path',
                  description:
                    'delete the field or element with specified path (for JSON arrays, negative integers count from the end)',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_delete_at_path_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_delete_elem',
                  description:
                    'delete the array element with specified index (negative integers count from the end). throws an error if top level container is not an array',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_delete_elem_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_delete_key',
                  description:
                    'delete key/value pair or string element. key/value pairs are matched based on their key value',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_delete_key_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_inc',
                  description:
                    'increments the numeric columns with given value of the filtered values',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_inc_input',
                    ofType: null,
                  },
                  defaultValue: null,
                },
                {
                  name: '_prepend',
                  description:
                    'prepend existing jsonb value of filtered columns with new jsonb value',
                  type: {
                    kind: 'INPUT_OBJECT',
                    name: 'Stuff_prepend_input',
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
                    name: 'Stuff_set_input',
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
                      name: 'Stuff_pk_columns_input',
                      ofType: null,
                    },
                  },
                  defaultValue: null,
                },
              ],
              type: {
                kind: 'OBJECT',
                name: 'Stuff',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'update_Stuff_many',
              description: 'update multiples rows of table: "Stuff"',
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
                          name: 'Stuff_updates',
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
                  name: 'Stuff_mutation_response',
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
          kind: 'INPUT_OBJECT',
          name: 'number_mysql8_comparison_exp',
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
              name: 'Author',
              description: 'fetch data from the table: "Author"',
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
                        name: 'Author_select_column',
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
                        name: 'Author_order_by',
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
                    name: 'Author_bool_exp',
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
                      name: 'Author',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Author_aggregate',
              description: 'fetch aggregated fields from the table: "Author"',
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
                        name: 'Author_select_column',
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
                        name: 'Author_order_by',
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
                    name: 'Author_bool_exp',
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
                  name: 'Author_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Author_by_pk',
              description:
                'fetch data from the table: "Author" using primary key columns',
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
                name: 'Author',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Cart',
              description: 'fetch data from the table: "Cart"',
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
                        name: 'Cart_select_column',
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
                        name: 'Cart_order_by',
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
                    name: 'Cart_bool_exp',
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
                      name: 'Cart',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Cart_aggregate',
              description: 'fetch aggregated fields from the table: "Cart"',
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
                        name: 'Cart_select_column',
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
                        name: 'Cart_order_by',
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
                    name: 'Cart_bool_exp',
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
                  name: 'Cart_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Cart_by_pk',
              description:
                'fetch data from the table: "Cart" using primary key columns',
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
                name: 'Cart',
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
                      name: 'number',
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
              name: 'Record',
              description: 'fetch data from the table: "Record"',
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
                        name: 'Record_select_column',
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
                        name: 'Record_order_by',
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
                    name: 'Record_bool_exp',
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
                      name: 'Record',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Record_aggregate',
              description: 'fetch aggregated fields from the table: "Record"',
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
                        name: 'Record_select_column',
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
                        name: 'Record_order_by',
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
                    name: 'Record_bool_exp',
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
                  name: 'Record_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Record_by_pk',
              description:
                'fetch data from the table: "Record" using primary key columns',
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
                name: 'Record',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Stuff',
              description: 'fetch data from the table: "Stuff"',
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
                        name: 'Stuff_select_column',
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
                        name: 'Stuff_order_by',
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
                    name: 'Stuff_bool_exp',
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
                      name: 'Stuff',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Stuff_aggregate',
              description: 'fetch aggregated fields from the table: "Stuff"',
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
                        name: 'Stuff_select_column',
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
                        name: 'Stuff_order_by',
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
                    name: 'Stuff_bool_exp',
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
                  name: 'Stuff_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Stuff_by_pk',
              description:
                'fetch data from the table: "Stuff" using primary key columns',
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
                name: 'Stuff',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Track',
              description: 'fetch data from the table: "Track"',
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
              name: 'Track_aggregate',
              description: 'fetch aggregated fields from the table: "Track"',
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
            {
              name: 'Track_by_pk',
              description:
                'fetch data from the table: "Track" using primary key columns',
              args: [
                {
                  name: 'TrackId',
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
                name: 'Track',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'samples_for_documentation_articles',
              description:
                'fetch data from the table: "samples_for_documentation.articles"',
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
                        name: 'samples_for_documentation_articles_select_column',
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
                        name: 'samples_for_documentation_articles_order_by',
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
                    name: 'samples_for_documentation_articles_bool_exp',
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
                      name: 'samples_for_documentation_articles',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'samples_for_documentation_articles_aggregate',
              description:
                'fetch aggregated fields from the table: "samples_for_documentation.articles"',
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
                        name: 'samples_for_documentation_articles_select_column',
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
                        name: 'samples_for_documentation_articles_order_by',
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
                    name: 'samples_for_documentation_articles_bool_exp',
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
                  name: 'samples_for_documentation_articles_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'samples_for_documentation_authors',
              description:
                'fetch data from the table: "samples_for_documentation.authors"',
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
                        name: 'samples_for_documentation_authors_select_column',
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
                        name: 'samples_for_documentation_authors_order_by',
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
                    name: 'samples_for_documentation_authors_bool_exp',
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
                      name: 'samples_for_documentation_authors',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'samples_for_documentation_authors_aggregate',
              description:
                'fetch aggregated fields from the table: "samples_for_documentation.authors"',
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
                        name: 'samples_for_documentation_authors_select_column',
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
                        name: 'samples_for_documentation_authors_order_by',
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
                    name: 'samples_for_documentation_authors_bool_exp',
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
                  name: 'samples_for_documentation_authors_aggregate',
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
          name: 'samples_for_documentation_articles',
          description:
            'columns and relationships of "samples_for_documentation.articles"',
          fields: [
            {
              name: 'author_id',
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
              name: 'body',
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
              name: 'published_on',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Datetime',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'title',
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
          name: 'samples_for_documentation_articles_aggregate',
          description:
            'aggregated selection of "samples_for_documentation.articles"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'samples_for_documentation_articles_aggregate_fields',
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
                      name: 'samples_for_documentation_articles',
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
          name: 'samples_for_documentation_articles_aggregate_fields',
          description:
            'aggregate fields of "samples_for_documentation.articles"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'samples_for_documentation_articles_avg_fields',
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
                        name: 'samples_for_documentation_articles_select_column',
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
                name: 'samples_for_documentation_articles_max_fields',
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
                name: 'samples_for_documentation_articles_min_fields',
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
                name: 'samples_for_documentation_articles_stddev_fields',
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
                name: 'samples_for_documentation_articles_stddev_pop_fields',
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
                name: 'samples_for_documentation_articles_stddev_samp_fields',
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
                name: 'samples_for_documentation_articles_sum_fields',
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
                name: 'samples_for_documentation_articles_var_pop_fields',
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
                name: 'samples_for_documentation_articles_var_samp_fields',
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
                name: 'samples_for_documentation_articles_variance_fields',
                ofType: null,
              },
              isDeprecated: false,
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
          name: 'samples_for_documentation_articles_avg_fields',
          description: 'aggregate avg on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'samples_for_documentation_articles_bool_exp',
          description:
            'Boolean expression to filter rows from the table "samples_for_documentation.articles". All fields are combined with a logical \'AND\'.',
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
                    name: 'samples_for_documentation_articles_bool_exp',
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
                name: 'samples_for_documentation_articles_bool_exp',
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
                    name: 'samples_for_documentation_articles_bool_exp',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: 'author_id',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'body',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'published_on',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Datetime_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'title',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
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
          name: 'samples_for_documentation_articles_max_fields',
          description: 'aggregate max on columns',
          fields: [
            {
              name: 'author_id',
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
              name: 'body',
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
              name: 'published_on',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Datetime',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'title',
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
          name: 'samples_for_documentation_articles_min_fields',
          description: 'aggregate min on columns',
          fields: [
            {
              name: 'author_id',
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
              name: 'body',
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
              name: 'published_on',
              description: null,
              args: [],
              type: {
                kind: 'SCALAR',
                name: 'Datetime',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'title',
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
          name: 'samples_for_documentation_articles_order_by',
          description:
            'Ordering options when selecting data from "samples_for_documentation.articles".',
          fields: null,
          inputFields: [
            {
              name: 'author_id',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'body',
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
              name: 'published_on',
              description: null,
              type: {
                kind: 'ENUM',
                name: 'order_by',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'title',
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
          name: 'samples_for_documentation_articles_select_column',
          description:
            'select columns of table "samples_for_documentation.articles"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
            {
              name: 'author_id',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'body',
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
              name: 'published_on',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'title',
              description: 'column name',
              isDeprecated: false,
              deprecationReason: null,
            },
          ],
          possibleTypes: null,
        },
        {
          kind: 'OBJECT',
          name: 'samples_for_documentation_articles_stddev_fields',
          description: 'aggregate stddev on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'samples_for_documentation_articles_stddev_pop_fields',
          description: 'aggregate stddev_pop on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'samples_for_documentation_articles_stddev_samp_fields',
          description: 'aggregate stddev_samp on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'samples_for_documentation_articles_sum_fields',
          description: 'aggregate sum on columns',
          fields: [
            {
              name: 'author_id',
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
          kind: 'OBJECT',
          name: 'samples_for_documentation_articles_var_pop_fields',
          description: 'aggregate var_pop on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'samples_for_documentation_articles_var_samp_fields',
          description: 'aggregate var_samp on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'samples_for_documentation_articles_variance_fields',
          description: 'aggregate variance on columns',
          fields: [
            {
              name: 'author_id',
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
          name: 'samples_for_documentation_authors',
          description:
            'columns and relationships of "samples_for_documentation.authors"',
          fields: [
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
          name: 'samples_for_documentation_authors_aggregate',
          description:
            'aggregated selection of "samples_for_documentation.authors"',
          fields: [
            {
              name: 'aggregate',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'samples_for_documentation_authors_aggregate_fields',
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
                      name: 'samples_for_documentation_authors',
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
          name: 'samples_for_documentation_authors_aggregate_fields',
          description:
            'aggregate fields of "samples_for_documentation.authors"',
          fields: [
            {
              name: 'avg',
              description: null,
              args: [],
              type: {
                kind: 'OBJECT',
                name: 'samples_for_documentation_authors_avg_fields',
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
                        name: 'samples_for_documentation_authors_select_column',
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
                name: 'samples_for_documentation_authors_max_fields',
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
                name: 'samples_for_documentation_authors_min_fields',
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
                name: 'samples_for_documentation_authors_stddev_fields',
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
                name: 'samples_for_documentation_authors_stddev_pop_fields',
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
                name: 'samples_for_documentation_authors_stddev_samp_fields',
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
                name: 'samples_for_documentation_authors_sum_fields',
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
                name: 'samples_for_documentation_authors_var_pop_fields',
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
                name: 'samples_for_documentation_authors_var_samp_fields',
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
                name: 'samples_for_documentation_authors_variance_fields',
                ofType: null,
              },
              isDeprecated: false,
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
          name: 'samples_for_documentation_authors_avg_fields',
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
          name: 'samples_for_documentation_authors_bool_exp',
          description:
            'Boolean expression to filter rows from the table "samples_for_documentation.authors". All fields are combined with a logical \'AND\'.',
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
                    name: 'samples_for_documentation_authors_bool_exp',
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
                name: 'samples_for_documentation_authors_bool_exp',
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
                    name: 'samples_for_documentation_authors_bool_exp',
                    ofType: null,
                  },
                },
              },
              defaultValue: null,
            },
            {
              name: 'id',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'Int_BigQuery_comparison_exp',
                ofType: null,
              },
              defaultValue: null,
            },
            {
              name: 'name',
              description: null,
              type: {
                kind: 'INPUT_OBJECT',
                name: 'String_BigQuery_comparison_exp',
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
          name: 'samples_for_documentation_authors_max_fields',
          description: 'aggregate max on columns',
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
          name: 'samples_for_documentation_authors_min_fields',
          description: 'aggregate min on columns',
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
          name: 'samples_for_documentation_authors_order_by',
          description:
            'Ordering options when selecting data from "samples_for_documentation.authors".',
          fields: null,
          inputFields: [
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
          kind: 'ENUM',
          name: 'samples_for_documentation_authors_select_column',
          description:
            'select columns of table "samples_for_documentation.authors"',
          fields: null,
          inputFields: null,
          interfaces: null,
          enumValues: [
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
          kind: 'OBJECT',
          name: 'samples_for_documentation_authors_stddev_fields',
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
          name: 'samples_for_documentation_authors_stddev_pop_fields',
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
          name: 'samples_for_documentation_authors_stddev_samp_fields',
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
          kind: 'OBJECT',
          name: 'samples_for_documentation_authors_sum_fields',
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
          kind: 'OBJECT',
          name: 'samples_for_documentation_authors_var_pop_fields',
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
          name: 'samples_for_documentation_authors_var_samp_fields',
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
          name: 'samples_for_documentation_authors_variance_fields',
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
          kind: 'INPUT_OBJECT',
          name: 'string_mysql8_comparison_exp',
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
              name: 'Author',
              description: 'fetch data from the table: "Author"',
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
                        name: 'Author_select_column',
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
                        name: 'Author_order_by',
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
                    name: 'Author_bool_exp',
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
                      name: 'Author',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Author_aggregate',
              description: 'fetch aggregated fields from the table: "Author"',
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
                        name: 'Author_select_column',
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
                        name: 'Author_order_by',
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
                    name: 'Author_bool_exp',
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
                  name: 'Author_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Author_by_pk',
              description:
                'fetch data from the table: "Author" using primary key columns',
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
                name: 'Author',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Author_stream',
              description:
                'fetch data from the table in a streaming manner: "Author"',
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
                        name: 'Author_stream_cursor_input',
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
                    name: 'Author_bool_exp',
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
                      name: 'Author',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Cart',
              description: 'fetch data from the table: "Cart"',
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
                        name: 'Cart_select_column',
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
                        name: 'Cart_order_by',
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
                    name: 'Cart_bool_exp',
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
                      name: 'Cart',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Cart_aggregate',
              description: 'fetch aggregated fields from the table: "Cart"',
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
                        name: 'Cart_select_column',
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
                        name: 'Cart_order_by',
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
                    name: 'Cart_bool_exp',
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
                  name: 'Cart_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Cart_by_pk',
              description:
                'fetch data from the table: "Cart" using primary key columns',
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
                name: 'Cart',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Cart_stream',
              description:
                'fetch data from the table in a streaming manner: "Cart"',
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
                        name: 'Cart_stream_cursor_input',
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
                    name: 'Cart_bool_exp',
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
                      name: 'Cart',
                      ofType: null,
                    },
                  },
                },
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
                      name: 'number',
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
              name: 'Record',
              description: 'fetch data from the table: "Record"',
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
                        name: 'Record_select_column',
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
                        name: 'Record_order_by',
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
                    name: 'Record_bool_exp',
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
                      name: 'Record',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Record_aggregate',
              description: 'fetch aggregated fields from the table: "Record"',
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
                        name: 'Record_select_column',
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
                        name: 'Record_order_by',
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
                    name: 'Record_bool_exp',
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
                  name: 'Record_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Record_by_pk',
              description:
                'fetch data from the table: "Record" using primary key columns',
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
                name: 'Record',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Record_stream',
              description:
                'fetch data from the table in a streaming manner: "Record"',
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
                        name: 'Record_stream_cursor_input',
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
                    name: 'Record_bool_exp',
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
                      name: 'Record',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Stuff',
              description: 'fetch data from the table: "Stuff"',
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
                        name: 'Stuff_select_column',
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
                        name: 'Stuff_order_by',
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
                    name: 'Stuff_bool_exp',
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
                      name: 'Stuff',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Stuff_aggregate',
              description: 'fetch aggregated fields from the table: "Stuff"',
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
                        name: 'Stuff_select_column',
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
                        name: 'Stuff_order_by',
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
                    name: 'Stuff_bool_exp',
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
                  name: 'Stuff_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Stuff_by_pk',
              description:
                'fetch data from the table: "Stuff" using primary key columns',
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
                name: 'Stuff',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Stuff_stream',
              description:
                'fetch data from the table in a streaming manner: "Stuff"',
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
                        name: 'Stuff_stream_cursor_input',
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
                    name: 'Stuff_bool_exp',
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
                      name: 'Stuff',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'Track',
              description: 'fetch data from the table: "Track"',
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
              name: 'Track_aggregate',
              description: 'fetch aggregated fields from the table: "Track"',
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
            {
              name: 'Track_by_pk',
              description:
                'fetch data from the table: "Track" using primary key columns',
              args: [
                {
                  name: 'TrackId',
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
                name: 'Track',
                ofType: null,
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'samples_for_documentation_articles',
              description:
                'fetch data from the table: "samples_for_documentation.articles"',
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
                        name: 'samples_for_documentation_articles_select_column',
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
                        name: 'samples_for_documentation_articles_order_by',
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
                    name: 'samples_for_documentation_articles_bool_exp',
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
                      name: 'samples_for_documentation_articles',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'samples_for_documentation_articles_aggregate',
              description:
                'fetch aggregated fields from the table: "samples_for_documentation.articles"',
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
                        name: 'samples_for_documentation_articles_select_column',
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
                        name: 'samples_for_documentation_articles_order_by',
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
                    name: 'samples_for_documentation_articles_bool_exp',
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
                  name: 'samples_for_documentation_articles_aggregate',
                  ofType: null,
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'samples_for_documentation_authors',
              description:
                'fetch data from the table: "samples_for_documentation.authors"',
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
                        name: 'samples_for_documentation_authors_select_column',
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
                        name: 'samples_for_documentation_authors_order_by',
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
                    name: 'samples_for_documentation_authors_bool_exp',
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
                      name: 'samples_for_documentation_authors',
                      ofType: null,
                    },
                  },
                },
              },
              isDeprecated: false,
              deprecationReason: null,
            },
            {
              name: 'samples_for_documentation_authors_aggregate',
              description:
                'fetch aggregated fields from the table: "samples_for_documentation.authors"',
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
                        name: 'samples_for_documentation_authors_select_column',
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
                        name: 'samples_for_documentation_authors_order_by',
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
                    name: 'samples_for_documentation_authors_bool_exp',
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
                  name: 'samples_for_documentation_authors_aggregate',
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

export const schema = new GraphQLSchema(
  buildClientSchema(
    introspection.data as unknown as IntrospectionQuery
  ).toConfig()
);
