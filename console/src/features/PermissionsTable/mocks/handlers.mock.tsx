import { rest } from 'msw';

const url = 'http://localhost:6006';

export const handlers = [
  rest.post(`${url}/v2/query`, (req, res, ctx) => {
    const body = req.body as Record<string, any>;

    const isUseSchemaList = body?.args?.sql?.includes('SELECT schema_name');

    if (isUseSchemaList) {
      return res(
        ctx.json({
          result_type: 'TuplesOk',
          result: [['schema_name'], ['public'], ['default']],
        })
      );
    }

    return res(
      ctx.json([
        {
          result_type: 'TuplesOk',
          result: [
            ['tables'],
            [
              '[{"table_schema":"public","table_name":"users","table_type":"TABLE","comment":"hias","columns":[{"comment": null, "data_type": "integer", "table_name": "users", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "description", "is_nullable": "YES", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 3}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "name", "is_nullable": "YES", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}, {"table_schema":"default","table_name":"users","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "users", "column_name": "id", "is_nullable": "NO", "table_schema": "default", "column_default": "nextval(\'\\"default\\".users_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "name", "is_nullable": "NO", "table_schema": "default", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}]',
            ],
          ],
        },
        { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
        {
          result_type: 'TuplesOk',
          result: [
            ['coalesce'],
            [
              '[{"table_schema":"default","table_name":"users","constraint_name":"users_pkey","columns":["id"]}, {"table_schema":"public","table_name":"users","constraint_name":"users_pkey","columns":["id"]}]',
            ],
          ],
        },
        { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
        { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
      ])
    );
  }),

  rest.post(`${url}/v1/metadata`, (req, res, ctx) => {
    const body = req.body as Record<string, any>;
    if (body.type === 'export_metadata') {
      return res(
        ctx.json({
          resource_version: 380,
          metadata: {
            version: 3,
            sources: [
              {
                name: 'default',
                kind: 'postgres',
                tables: [
                  {
                    table: { schema: 'public', name: 'users' },
                    insert_permissions: [
                      {
                        role: 'user',
                        permission: {
                          check: {},
                          columns: ['id', 'name'],
                          backend_only: false,
                        },
                      },
                    ],
                    select_permissions: [
                      {
                        role: 'user',
                        permission: {
                          columns: ['id', 'description', 'name'],
                          filter: {},
                          limit: 0,
                        },
                      },
                    ],
                  },
                ],
                functions: [{ function: { schema: 'public', name: 'me' } }],
                configuration: {
                  connection_info: {
                    use_prepared_statements: false,
                    database_url:
                      'postgres://postgres:postgrespassword@postgres:5432/postgres',
                    isolation_level: 'read-committed',
                  },
                },
              },
            ],
          },
        })
      );
    }

    return res(ctx.json([{ message: 'success' }]));
  }),
];
