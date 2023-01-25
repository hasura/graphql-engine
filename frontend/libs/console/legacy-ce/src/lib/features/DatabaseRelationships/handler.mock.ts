import { Metadata } from '@/features/hasura-metadata-types';
import { rest } from 'msw';

const mockMetadata: Metadata = {
  resource_version: 143,
  metadata: {
    version: 3,
    sources: [
      {
        name: 'bikes',
        kind: 'mssql',
        tables: [
          {
            table: {
              name: 'products',
              schema: 'production',
            },
            object_relationships: [
              {
                name: 'fk_object_rel',
                using: {
                  foreign_key_constraint_on: 'brand_id',
                },
              },
              {
                name: 'manual_obj_local_rel',
                using: {
                  manual_configuration: {
                    column_mapping: {
                      product_id: 'brand_id',
                    },
                    insertion_order: null,
                    remote_table: {
                      name: 'brands',
                      schema: 'production',
                    },
                  },
                },
              },
            ],
            array_relationships: [
              {
                name: 'manual_array_rel',
                using: {
                  manual_configuration: {
                    column_mapping: {
                      product_name: 'brand_name',
                    },
                    remote_table: {
                      name: 'brands',
                      schema: 'production',
                    },
                  },
                },
              },
              {
                name: 'fk_array_rel',
                using: {
                  foreign_key_constraint_on: {
                    column: 'product_id',
                    table: {
                      name: 'stocks',
                      schema: 'production',
                    },
                  },
                },
              },
            ],
          },
          {
            table: {
              name: 'stocks',
              schema: 'production',
            },
            object_relationships: [
              {
                name: 'product',
                using: {
                  foreign_key_constraint_on: 'product_id',
                },
              },
            ],
          },
        ],
        configuration: {
          connection_info: {
            connection_string:
              'DRIVER={ODBC Driver 17 for SQL Server};SERVER=host.docker.internal;DATABASE=bikes;Uid=SA;Pwd=reallyStrongPwd123',
            pool_settings: {
              idle_timeout: 5,
              max_connections: 50,
            },
          },
        },
      },
    ],
    backend_configs: {
      dataconnector: {
        sqlite: {
          uri: 'http://host.docker.internal:8100',
        },
      },
    },
  },
};

const mockQueryResponse = {
  result_type: 'TuplesOk',
  result: [
    ['fromTable', 'fromSchema', 'toTable', 'toSchema', 'column_mapping'],
    [
      'products',
      'production',
      'categories',
      'production',
      '[{"column":"category_id","referenced_column":"category_id"}]',
    ],
    [
      'products',
      'production',
      'brands',
      'production',
      '[{"column":"brand_id","referenced_column":"brand_id"}]',
    ],
    [
      'order_items',
      'sales',
      'products',
      'production',
      '[{"column":"product_id","referenced_column":"product_id"}]',
    ],
    [
      'stocks',
      'production',
      'products',
      'production',
      '[{"column":"product_id","referenced_column":"product_id"}]',
    ],
  ],
};

export const handlers = () => [
  rest.post(`http://localhost:8080/v1/metadata`, (req, res, ctx) => {
    const requestBody = req.body as Record<string, any>;

    if (requestBody.type === 'export_metadata')
      return res(ctx.json(mockMetadata));

    return res(ctx.json({}));
  }),
  rest.post(`http://localhost:8080/v2/query`, (req, res, ctx) => {
    // const requestBody = req.body as Record<string, any>;

    // if (requestBody.type === 'export_metadata')
    //   return res(ctx.json(mockMetadata));

    return res(ctx.json(mockQueryResponse));
  }),
];
