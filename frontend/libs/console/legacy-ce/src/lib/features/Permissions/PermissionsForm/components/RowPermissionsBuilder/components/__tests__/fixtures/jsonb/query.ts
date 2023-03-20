export const queries = [
  {
    payload: {
      type: 'run_sql',
      args: {
        source: 'default',
        sql: 'SELECT version();',
        cascade: false,
        read_only: true,
      },
    },
    response: {
      result_type: 'TuplesOk',
      result: [
        ['version'],
        [
          'PostgreSQL 15.2 (Debian 15.2-1.pgdg110+1) on x86_64-pc-linux-gnu, compiled by gcc (Debian 10.2.1-6) 10.2.1 20210110, 64-bit',
        ],
      ],
    },
  },
  {
    payload: {
      type: 'run_sql',
      args: {
        sql: "\n  SELECT \n   column_name, data_type, is_nullable\n  FROM \n    information_schema.columns \n  WHERE \n    table_schema = 'public' AND \n    table_name  = 'Stuff';",
        source: 'default',
      },
    },
    response: {
      result_type: 'TuplesOk',
      result: [
        ['column_name', 'data_type', 'is_nullable'],
        ['id', 'integer', 'NO'],
        ['name', 'text', 'NO'],
        ['enabled', 'boolean', 'YES'],
        ['cart_id', 'integer', 'YES'],
        ['jason', 'jsonb', 'YES'],
        ['box', 'box', 'YES'],
      ],
    },
  },
  {
    payload: {
      type: 'run_sql',
      args: {
        sql: 'SELECT a.attname\n  FROM   pg_index i\n  JOIN   pg_attribute a ON a.attrelid = i.indrelid\n                       AND a.attnum = ANY(i.indkey)\n  WHERE  i.indrelid = \'public."Stuff"\'::regclass\n  AND    i.indisprimary;',
        source: 'default',
      },
    },
    response: { result_type: 'TuplesOk', result: [['attname'], ['id']] },
  },
  {
    payload: {
      type: 'run_sql',
      args: {
        sql: "\n  SELECT \n   column_name, data_type, is_nullable\n  FROM \n    information_schema.columns \n  WHERE \n    table_schema = 'public' AND \n    table_name  = 'Stuff';",
        source: 'default',
      },
    },
    response: {
      result_type: 'TuplesOk',
      result: [
        ['column_name', 'data_type', 'is_nullable'],
        ['id', 'integer', 'NO'],
        ['name', 'text', 'NO'],
        ['enabled', 'boolean', 'YES'],
        ['cart_id', 'integer', 'YES'],
        ['jason', 'jsonb', 'YES'],
        ['box', 'box', 'YES'],
      ],
    },
  },
  {
    payload: {
      type: 'run_sql',
      args: {
        sql: 'SELECT a.attname\n  FROM   pg_index i\n  JOIN   pg_attribute a ON a.attrelid = i.indrelid\n                       AND a.attnum = ANY(i.indkey)\n  WHERE  i.indrelid = \'public."Stuff"\'::regclass\n  AND    i.indisprimary;',
        source: 'default',
      },
    },
    response: { result_type: 'TuplesOk', result: [['attname'], ['id']] },
  },
  {
    payload: {
      type: 'run_sql',
      args: {
        sql: "\n  SELECT \n   column_name, data_type, is_nullable\n  FROM \n    information_schema.columns \n  WHERE \n    table_schema = 'public' AND \n    table_name  = 'Stuff';",
        source: 'default',
      },
    },
    response: {
      result_type: 'TuplesOk',
      result: [
        ['column_name', 'data_type', 'is_nullable'],
        ['id', 'integer', 'NO'],
        ['name', 'text', 'NO'],
        ['enabled', 'boolean', 'YES'],
        ['cart_id', 'integer', 'YES'],
        ['jason', 'jsonb', 'YES'],
        ['box', 'box', 'YES'],
      ],
    },
  },
  {
    payload: {
      type: 'run_sql',
      args: {
        sql: "\n  WITH partitions as (\n    SELECT array(\n      SELECT\n      child.relname       AS partition\n  FROM pg_inherits\n      JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid\n      JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace\n    ) as names\n  )\n  SELECT info_schema.table_name, info_schema.table_schema, info_schema.table_type \n  FROM information_schema.tables as info_schema, partitions\n  WHERE \n    info_schema.table_schema NOT IN ('information_schema', 'pg_catalog', 'hdb_catalog', '_timescaledb_internal')\n    AND NOT (info_schema.table_name = ANY (partitions.names))     \n  ",
        source: 'default',
      },
    },
    response: {
      result_type: 'TuplesOk',
      result: [
        ['table_name', 'table_schema', 'table_type'],
        ['Stuff', 'public', 'BASE TABLE'],
        ['Author', 'public', 'BASE TABLE'],
        ['Cart', 'public', 'BASE TABLE'],
        ['Album', 'public', 'BASE TABLE'],
        ['Record', 'public', 'BASE TABLE'],
      ],
    },
  },
];
