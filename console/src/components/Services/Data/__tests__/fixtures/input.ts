export const citus_no_tables = {
  query_data: [
    { result_type: 'TuplesOk', result: [['tables'], ['[]']] },
    { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
    { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
    { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
    { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
  ] as any,
  metadata: [],
};

export const citus_with_table = {
  query_data: [
    {
      result_type: 'TuplesOk',
      result: [
        ['tables'],
        [
          '[{"table_schema":"public","table_name":"users","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "users", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": "nextval(\'users_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "name", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null,"citus_table_type":"local"}]',
        ],
      ],
    },
    { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
    {
      result_type: 'TuplesOk',
      result: [
        ['coalesce'],
        [
          '[{"table_schema":"public","table_name":"users","constraint_name":"users_pkey","columns":["id"]}]',
        ],
      ],
    },
    { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
    { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
  ] as any,
  metadata: [{ table: { schema: 'public', name: 'users' } }],
};

export const citus_with_relationships_fk = {
  query_data: [
    {
      result_type: 'TuplesOk',
      result: [
        ['tables'],
        [
          '[{"table_schema":"public","table_name":"users","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "users", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": "nextval(\'users_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "users", "column_name": "name", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null,"citus_table_type":"local"}, {"table_schema":"public","table_name":"posts","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "posts", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": "nextval(\'posts_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "integer", "table_name": "posts", "column_name": "user_id", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "int4", "ordinal_position": 3}, {"comment": null, "data_type": "text", "table_name": "posts", "column_name": "post", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null,"citus_table_type":"local"}]',
        ],
      ],
    },
    {
      result_type: 'TuplesOk',
      result: [
        ['coalesce'],
        [
          '[{"table_schema":"public","table_name":"posts","constraint_name":"posts_user_id_fkey","ref_table_table_schema":"public","ref_table":"users","column_mapping":{ "user_id" : "id" },"on_update":"r","on_delete":"r"}]',
        ],
      ],
    },
    {
      result_type: 'TuplesOk',
      result: [
        ['coalesce'],
        [
          '[{"table_schema":"public","table_name":"posts","constraint_name":"posts_pkey","columns":["id"]}, {"table_schema":"public","table_name":"users","constraint_name":"users_pkey","columns":["id"]}]',
        ],
      ],
    },
    { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
    { result_type: 'TuplesOk', result: [['coalesce'], ['[]']] },
  ] as any,
  metadata: [
    {
      table: { schema: 'public', name: 'posts' },
      object_relationships: [
        { name: 'user', using: { foreign_key_constraint_on: 'user_id' } },
      ],
    },
    {
      table: { schema: 'public', name: 'users' },
      array_relationships: [
        {
          name: 'posts',
          using: {
            foreign_key_constraint_on: {
              column: 'user_id',
              table: { schema: 'public', name: 'posts' },
            },
          },
        },
      ],
    },
  ],
};
