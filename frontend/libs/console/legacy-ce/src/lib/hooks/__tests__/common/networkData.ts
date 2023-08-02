import { RunSQLResponse } from '../../types';

/* eslint-disable no-useless-escape */
export const ErrorResponse = {
  unknownSource: {
    path: '$',
    error: 'source with name "secon" does not exist',
    code: 'not-exists',
  },
  wrongDriver: {
    path: '$',
    error: 'source with name "default" is inconsistent',
    code: 'unexpected',
  },
};

export const MetadataNetworkData = {
  constant:
    '{"resource_version":196,"metadata":{"version":3,"sources":[{"name":"default","kind":"postgres","tables":[{"table":{"schema":"export","name":"person"}},{"table":{"schema":"public","name":"person"}}],"configuration":{"connection_info":{"use_prepared_statements":false,"database_url":{"from_env":"HASURA_GRAPHQL_DATABASE_URL"},"isolation_level":"read-committed"}}},{"name":"citus","kind":"citus","tables":[{"table":{"schema":"public","name":"comrades"}}],"configuration":{"connection_info":{"use_prepared_statements":false,"database_url":"postgresql://postgres:postgres@host.docker.internal:6543/postgres","isolation_level":"read-committed"}}},{"name":"mssql","kind":"mssql","tables":[{"table":{"schema":"dbo","name":"besties"}},{"table":{"schema":"dbo","name":"flights"}},{"table":{"schema":"sales_schema","name":"sales"}},{"table":{"schema":"dbo","name":"testers"}}],"configuration":{"connection_info":{"connection_string":"DRIVER={ODBC Driver 18 for SQL Server};SERVER=172.17.0.1;DATABASE=master;Uid=SA;Pwd=reallyStrongPwd123;Encrypt=optional","pool_settings":{"idle_timeout":5,"max_connections":50}}}},{"name":"second","kind":"postgres","tables":[{"table":{"schema":"authors_schema","name":"authors"}},{"table":{"schema":"books_schema","name":"books"}},{"table":{"schema":"books_schema","name":"test3"}},{"table":{"schema":"public","name":"test2"}},{"table":{"schema":"public","name":"user_book"}},{"table":{"schema":"public","name":"users"}}],"configuration":{"connection_info":{"use_prepared_statements":false,"database_url":"postgresql://postgres:postgres@host.docker.internal:5432/postgres","isolation_level":"read-committed"}}}],"actions":[{"name":"actionName","definition":{"handler":"http://host.docker.internal:3000","output_type":"SampleOutput","arguments":[{"name":"arg1","type":"SampleInput!"}],"type":"mutation","kind":"synchronous"},"transform":null,"permissions":[{"role":"editor"},{"role":"user"}]}],"custom_types":{"input_objects":[{"name":"SampleInput","fields":[{"name":"username","type":"String!"},{"name":"password","type":"String!"}]}],"objects":[{"name":"SampleOutput","fields":[{"name":"accessToken","type":"String!"}]}]}}}',
};
export interface NetworkDataType {
  single_primary_key: RunSQLResponse;
  multi_primary_key: RunSQLResponse;
  single_unique_key: RunSQLResponse;
  multi_unique_key: RunSQLResponse;
  single_foreign_key: RunSQLResponse;
  multi_foreign_key: RunSQLResponse;
  single_table: RunSQLResponse;
  multi_table: RunSQLResponse;
  single_check_constraint: RunSQLResponse;
  multi_check_constraint: RunSQLResponse;
  trackable_functions?: RunSQLResponse;
  'non-trackable_functions'?: RunSQLResponse;
  single_functions?: RunSQLResponse;
  schema_list: RunSQLResponse;
}

export const mssqlNetworkData: NetworkDataType = {
  single_primary_key: {
    result_type: 'TuplesOk',
    result: [
      ['JSON_F52E2B61-18A1-11d1-B105-00805F49916B'],
      [
        '[{"table_schema":"dbo","table_name":"name","constraints":[{"name":"id","constraint_name":"PK__name__25E7CFAC8E0F24E1"},{"name":"uuid","constraint_name":"PK__name__25E7CFAC8E0F24E1"}]}]',
      ],
    ],
  },
  multi_primary_key: {
    result_type: 'TuplesOk',
    result: [
      ['JSON_F52E2B61-18A1-11d1-B105-00805F49916B'],
      [
        '[{"table_schema":"dbo","table_name":"besties","constraints":[{"name":"id","constraint_name":"PK__besties__3213E83F85F84B28"}]},{"table_schema":"dbo","table_name":"flights","constraints":[{"name":"id","constraint_name":"PK__flights__3213E83F8438E01F"}]},{"table_schema":"dbo","table_name":"name","constraints":[{"name":"id","constraint_name":"PK__name__25E7CFAC8E0F24E1"},{"name":"uuid","constraint_name":"PK__name__25E7CFAC8E0F24E1"}]},{"table_schema":"sales_schema","table_name":"sales","constraints":[{"name":"id","constraint_name":"PK__sales__3213E83F9A348DC9"}]},{"table_schema":"dbo","table_name":"testers","constraints":[{"name":"id","constraint_name":"PK__testers__3213E83F161D5374"}]}]',
      ],
    ],
  },
  single_unique_key: {
    result_type: 'TuplesOk',
    result: [
      ['JSON_F52E2B61-18A1-11d1-B105-00805F49916B'],
      [
        '[{"table_schema":"dbo","table_name":"citizens","constraints":[{"name":"ID","constraint_name":"UC_Person"},{"name":"LastName","constraint_name":"UC_Person"}]}]',
      ],
    ],
  },
  multi_unique_key: {
    result_type: 'TuplesOk',
    result: [
      ['JSON_F52E2B61-18A1-11d1-B105-00805F49916B'],
      [
        '[{"table_schema":"dbo","table_name":"citizens","constraints":[{"name":"ID","constraint_name":"UC_Person"},{"name":"LastName","constraint_name":"UC_Person"}]}]',
      ],
    ],
  },
  single_foreign_key: {
    result_type: 'TuplesOk',
    result: [
      ['JSON_F52E2B61-18A1-11d1-B105-00805F49916B'],
      [
        '[{"constraint_name":"FK_flight","table_schema":"dbo","table_name":"testers","ref_table_schema":"dbo","ref_table":"flights","column_mapping":[{"column":"id","referenced_column":"id"}],"on_delete":"NO_ACTION","on_update":"NO_ACTION"},{"constraint_name":"FK_sale","table_schema":"dbo","table_name":"testers","ref_table_schema":"sales_schema","ref_table":"sales","column_mapping":[{"column":"id","referenced_column":"id"}],"on_delete":"NO_ACTION","on_update":"NO_ACTION"}]',
      ],
    ],
  },
  multi_foreign_key: {
    result_type: 'TuplesOk',
    result: [
      ['JSON_F52E2B61-18A1-11d1-B105-00805F49916B'],
      [
        '[{"constraint_name":"FK_flights","table_schema":"sales_schema","table_name":"sales","ref_table_schema":"dbo","ref_table":"flights","column_mapping":[{"column":"id","referenced_column":"id"}],"on_delete":"CASCADE","on_update":"CASCADE"},{"constraint_name":"FK_flight","table_schema":"dbo","table_name":"testers","ref_table_schema":"dbo","ref_table":"flights","column_mapping":[{"column":"id","referenced_column":"id"}],"on_delete":"NO_ACTION","on_update":"NO_ACTION"},{"constraint_name":"FK_besties","table_schema":"sales_schema","table_name":"sales","ref_table_schema":"dbo","ref_table":"besties","column_mapping":[{"column":"id","referenced_column":"id"}],"on_delete":"CASCADE","on_update":"CASCADE"},{"constraint_name":"FK_sale","table_schema":"dbo","table_name":"testers","ref_table_schema":"sales_schema","ref_table":"sales","column_mapping":[{"column":"id","referenced_column":"id"}],"on_delete":"NO_ACTION","on_update":"NO_ACTION"}]',
      ],
    ],
  },
  single_table: {
    result_type: 'TuplesOk',
    result: [
      ['table_schema', 'table_name', 'table_type', '', 'columns'],
      [
        'dbo',
        'flights',
        'TABLE',
        '[{}]',
        '[{"column_name":"id","ordinal_position":1,"is_nullable":"NO","data_type":"OTHER","data_type_name":"int","table_schema":"dbo","table_name":"flights"},{"column_name":"origin","ordinal_position":2,"collation_name":"SQL_Latin1_General_CP1_CI_AS","is_nullable":"NO","data_type":"OTHER","data_type_name":"varchar","table_schema":"dbo","table_name":"flights"},{"column_name":"destination","ordinal_position":3,"collation_name":"SQL_Latin1_General_CP1_CI_AS","is_nullable":"NO","data_type":"OTHER","data_type_name":"varchar","table_schema":"dbo","table_name":"flights"},{"column_name":"duration","ordinal_position":4,"is_nullable":"NO","data_type":"OTHER","data_type_name":"int","table_schema":"dbo","table_name":"flights"}]',
      ],
    ],
  },
  multi_table: {
    result_type: 'TuplesOk',
    result: [
      ['table_schema', 'table_name', 'table_type', '', 'columns'],
      [
        'dbo',
        'flights',
        'TABLE',
        '[{}]',
        '[{"column_name":"id","ordinal_position":1,"is_nullable":"NO","data_type":"OTHER","data_type_name":"int","table_schema":"dbo","table_name":"flights"},{"column_name":"origin","ordinal_position":2,"collation_name":"SQL_Latin1_General_CP1_CI_AS","is_nullable":"NO","data_type":"OTHER","data_type_name":"varchar","table_schema":"dbo","table_name":"flights"},{"column_name":"destination","ordinal_position":3,"collation_name":"SQL_Latin1_General_CP1_CI_AS","is_nullable":"NO","data_type":"OTHER","data_type_name":"varchar","table_schema":"dbo","table_name":"flights"},{"column_name":"duration","ordinal_position":4,"is_nullable":"NO","data_type":"OTHER","data_type_name":"int","table_schema":"dbo","table_name":"flights"}]',
      ],
      [
        'dbo',
        'besties',
        'TABLE',
        '[{}]',
        '[{"column_name":"name","ordinal_position":1,"collation_name":"SQL_Latin1_General_CP1_CI_AS","is_nullable":"NO","data_type":"OTHER","data_type_name":"varchar","table_schema":"dbo","table_name":"besties"},{"column_name":"id","ordinal_position":2,"is_nullable":"NO","data_type":"OTHER","data_type_name":"int","table_schema":"dbo","table_name":"besties"}]',
      ],
      [
        'sales_schema',
        'sales',
        'TABLE',
        '[{}]',
        '[{"column_name":"id","ordinal_position":1,"is_nullable":"NO","data_type":"OTHER","data_type_name":"int","table_schema":"sales_schema","table_name":"sales"},{"column_name":"create_time","ordinal_position":2,"is_nullable":"YES","data_type":"OTHER","data_type_name":"datetime","table_schema":"sales_schema","table_name":"sales"},{"column_name":"update_time","ordinal_position":3,"is_nullable":"YES","data_type":"OTHER","data_type_name":"datetime","table_schema":"sales_schema","table_name":"sales"},{"column_name":"name","ordinal_position":4,"collation_name":"SQL_Latin1_General_CP1_CI_AS","is_nullable":"YES","data_type":"OTHER","data_type_name":"varchar","table_schema":"sales_schema","table_name":"sales"}]',
      ],
    ],
  },
  single_check_constraint: {
    result_type: 'TuplesOk',
    result: [
      ['JSON_F52E2B61-18A1-11d1-B105-00805F49916B'],
      [
        '[{"constraint_name":"CHK_Person","table_schema":"dbo","table_name":"ms_Persons1","column_name":null,"check":"([Age]>=(18) AND [City]=\'Sandnes\')"}]',
      ],
    ],
  },
  multi_check_constraint: {
    result_type: 'TuplesOk',
    result: [
      ['JSON_F52E2B61-18A1-11d1-B105-00805F49916B'],
      [
        '[{"constraint_name":"CHK_Person","table_schema":"dbo","table_name":"ms_Persons1","column_name":null,"check":"([Age]>=(18) AND [City]=\'Sandnes\')"},{"constraint_name":"CK__ms_Persons2__Age__4BCC3ABA","table_schema":"dbo","table_name":"ms_Persons2","column_name":"Age","check":"([Age]>=(18))"}]',
      ],
    ],
  },
  schema_list: {
    result_type: 'TuplesOk',
    result: [['schema_name'], ['dbo'], ['sales_schema']],
  },
};

export const postgresNetworkData: NetworkDataType = {
  single_primary_key: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"table_name":"person","table_schema":"public","constraint_name":"person_pkey","columns":["id"]}]',
      ],
    ],
  },
  multi_primary_key: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"table_name":"authors","table_schema":"authors_schema","constraint_name":"authors_pkey","columns":["id"]}, {"table_name":"books","table_schema":"books_schema","constraint_name":"books_pkey","columns":["id"]}, {"table_name":"car","table_schema":"public","constraint_name":"car_pkey","columns":["id"]}, {"table_name":"color","table_schema":"public","constraint_name":"color_pkey","columns":["id"]}, {"table_name":"house","table_schema":"public","constraint_name":"house_pkey","columns":["id"]}, {"table_name":"person","table_schema":"export","constraint_name":"person_pkey","columns":["id"]}, {"table_name":"person","table_schema":"public","constraint_name":"person_pkey","columns":["id"]}, {"table_name":"pet","table_schema":"public","constraint_name":"pet_pkey","columns":["id"]}]',
      ],
    ],
  },
  single_unique_key: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"table_name":"car","table_schema":"public","constraint_name":"car_person_id_key","columns":["person_id"]}]',
      ],
    ],
  },
  multi_unique_key: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"table_name":"car","table_schema":"public","constraint_name":"car_person_id_key","columns":["person_id"]}, {"table_name":"color","table_schema":"public","constraint_name":"color_person_id_key","columns":["person_id"]}]',
      ],
    ],
  },
  single_foreign_key: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"table_schema":"public","table_name":"car","constraint_name":"car_person_id_fkey","ref_table_table_schema":"public","ref_table":"person","column_mapping":{ "person_id" : "id" },"on_update":"a","on_delete":"a"}, {"table_schema":"public","table_name":"color","constraint_name":"color_person_id_fkey","ref_table_table_schema":"public","ref_table":"person","column_mapping":{ "person_id" : "id" },"on_update":"a","on_delete":"a"}, {"table_schema":"public","table_name":"pet","constraint_name":"pet_person_id_fkey","ref_table_table_schema":"public","ref_table":"person","column_mapping":{ "person_id" : "id" },"on_update":"a","on_delete":"a"}]',
      ],
    ],
  },
  multi_foreign_key: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"table_schema":"books_schema","table_name":"books","constraint_name":"author","ref_table_table_schema":"authors_schema","ref_table":"authors","column_mapping":{ "author_id" : "id" },"on_update":"a","on_delete":"a"}, {"table_schema":"public","table_name":"car","constraint_name":"car_person_id_fkey","ref_table_table_schema":"public","ref_table":"person","column_mapping":{ "person_id" : "id" },"on_update":"a","on_delete":"a"}, {"table_schema":"public","table_name":"color","constraint_name":"color_person_id_fkey","ref_table_table_schema":"public","ref_table":"person","column_mapping":{ "person_id" : "id" },"on_update":"a","on_delete":"a"}, {"table_schema":"public","table_name":"house","constraint_name":"house_person_id_fkey","ref_table_table_schema":"export","ref_table":"person","column_mapping":{ "person_id" : "id" },"on_update":"a","on_delete":"a"}, {"table_schema":"public","table_name":"pet","constraint_name":"pet_person_id_fkey","ref_table_table_schema":"public","ref_table":"person","column_mapping":{ "person_id" : "id" },"on_update":"a","on_delete":"a"}]',
      ],
    ],
  },
  single_table: {
    result_type: 'TuplesOk',
    result: [
      ['tables'],
      [
        '[{"table_schema":"public","table_name":"person","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "person", "column_name": "id", "is_nullable": "NO", "table_schema": "public", "column_default": null, "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "person", "column_name": "name", "is_nullable": "YES", "table_schema": "public", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}]',
      ],
    ],
  },
  multi_table: {
    result_type: 'TuplesOk',
    result: [
      ['tables'],
      [
        '[{"table_schema":"authors_schema","table_name":"authors","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "authors", "column_name": "id", "is_nullable": "NO", "table_schema": "authors_schema", "column_default": "nextval(\'authors_schema.authors_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "authors", "column_name": "name", "is_nullable": "NO", "table_schema": "authors_schema", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}, {"table_schema":"books_schema","table_name":"books","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "integer", "table_name": "books", "column_name": "author_id", "is_nullable": "NO", "table_schema": "books_schema", "column_default": null, "data_type_name": "int4", "ordinal_position": 3}, {"comment": null, "data_type": "integer", "table_name": "books", "column_name": "id", "is_nullable": "NO", "table_schema": "books_schema", "column_default": "nextval(\'books_schema.books_id_seq\'::regclass)", "data_type_name": "int4", "ordinal_position": 1}, {"comment": null, "data_type": "text", "table_name": "books", "column_name": "name", "is_nullable": "NO", "table_schema": "books_schema", "column_default": null, "data_type_name": "text", "ordinal_position": 2}],"triggers":[],"view_info":null}, {"table_schema":"export","table_name":"calculator","table_type":"TABLE","comment":null,"columns":[{"comment": null, "data_type": "numeric", "table_name": "calculator", "column_name": "value_one", "is_nullable": "YES", "table_schema": "export", "column_default": null, "data_type_name": "numeric", "ordinal_position": 1}, {"comment": null, "data_type": "numeric", "table_name": "calculator", "column_name": "value_two", "is_nullable": "YES", "table_schema": "export", "column_default": null, "data_type_name": "numeric", "ordinal_position": 2}],"triggers":[],"view_info":null}]',
      ],
    ],
  },
  single_check_constraint: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"table_schema":"public","table_name":"adults","constraint_name":"adults_age_check","column_name":"age","check":"CHECK (age > 18)"}]',
      ],
    ],
  },
  multi_check_constraint: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"table_schema":"public","table_name":"adults","constraint_name":"adults_age_check","column_name":"age","check":"CHECK (age > 18)"}, {"table_schema":"books_schema","table_name":"employees","constraint_name":"employees_birth_date_check","column_name":"birth_date","check":"CHECK (birth_date > \'1900-01-01\'::date)"}, {"table_schema":"books_schema","table_name":"employees","constraint_name":"employees_check","column_name":"birth_date","check":"CHECK (joined_date > birth_date)"}, {"table_schema":"books_schema","table_name":"employees","constraint_name":"employees_check","column_name":"joined_date","check":"CHECK (joined_date > birth_date)"}, {"table_schema":"books_schema","table_name":"employees","constraint_name":"employees_salary_check","column_name":"salary","check":"CHECK (salary > 0::numeric)"}, {"table_schema":"authors_schema","table_name":"persona","constraint_name":"chk_person","column_name":"age","check":"CHECK (age >= 18 AND city::text = \'Sandnes\'::text)"}, {"table_schema":"authors_schema","table_name":"persona","constraint_name":"chk_person","column_name":"city","check":"CHECK (age >= 18 AND city::text = \'Sandnes\'::text)"}]',
      ],
    ],
  },
  trackable_functions: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"function_name":"search_houses","function_schema":"public","description":null,"has_variadic":false,"function_type":"STABLE","function_definition":"CREATE OR REPLACE FUNCTION public.search_houses(search text)\\n RETURNS SETOF house\\n LANGUAGE sql\\n STABLE\\nAS $function$\\n    SELECT *\\n    FROM house\\n    WHERE\\n      name ilike (\'%\' || search || \'%\')\\n$function$\\n","return_type_schema":"public","return_type_name":"house","return_type_type":"c","comment":null,"returns_set":true,"input_arg_types":[{"schema" : "pg_catalog", "name" : "text", "type" : "b"}],"input_arg_names":["search"],"default_args":0,"function_oid":16697}, {"function_name":"search_persons","function_schema":"public","description":null,"has_variadic":false,"function_type":"STABLE","function_definition":"CREATE OR REPLACE FUNCTION public.search_persons(search text)\\n RETURNS SETOF person\\n LANGUAGE sql\\n STABLE\\nAS $function$\\n    SELECT *\\n    FROM public.person\\n    WHERE\\n      name ilike (\'%\' || search || \'%\')\\n$function$\\n","return_type_schema":"public","return_type_name":"person","return_type_type":"c","comment":null,"returns_set":true,"input_arg_types":[{"schema" : "pg_catalog", "name" : "text", "type" : "b"}],"input_arg_names":["search"],"default_args":0,"function_oid":16696}]',
      ],
    ],
  },
  'non-trackable_functions': {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"function_name":"armor","function_schema":"public","description":null,"has_variadic":false,"function_type":"IMMUTABLE","function_definition":"CREATE OR REPLACE FUNCTION public.armor(bytea, text[], text[])\\n RETURNS text\\n LANGUAGE c\\n IMMUTABLE PARALLEL SAFE STRICT\\nAS \'$libdir/pgcrypto\', $function$pg_armor$function$\\n","return_type_schema":"pg_catalog","return_type_name":"text","return_type_type":"b","comment":null,"returns_set":false,"input_arg_types":[{"schema" : "pg_catalog", "name" : "bytea", "type" : "b"}, {"schema" : "pg_catalog", "name" : "_text", "type" : "b"}, {"schema" : "pg_catalog", "name" : "_text", "type" : "b"}],"input_arg_names":[],"default_args":0,"function_oid":16419}, {"function_name":"armor","function_schema":"public","description":null,"has_variadic":false,"function_type":"IMMUTABLE","function_definition":"CREATE OR REPLACE FUNCTION public.armor(bytea)\\n RETURNS text\\n LANGUAGE c\\n IMMUTABLE PARALLEL SAFE STRICT\\nAS \'$libdir/pgcrypto\', $function$pg_armor$function$\\n","return_type_schema":"pg_catalog","return_type_name":"text","return_type_type":"b","comment":null,"returns_set":false,"input_arg_types":[{"schema" : "pg_catalog", "name" : "bytea", "type" : "b"}],"input_arg_names":[],"default_args":0,"function_oid":16418}]',
      ],
    ],
  },
  single_functions: {
    result_type: 'TuplesOk',
    result: [
      ['coalesce'],
      [
        '[{"function_name":"search_houses","function_schema":"public","description":null,"has_variadic":false,"function_type":"STABLE","function_definition":"CREATE OR REPLACE FUNCTION public.search_houses(search text)\\n RETURNS SETOF house\\n LANGUAGE sql\\n STABLE\\nAS $function$\\n    SELECT *\\n    FROM house\\n    WHERE\\n      name ilike (\'%\' || search || \'%\')\\n$function$\\n","return_type_schema":"public","return_type_name":"house","return_type_type":"c","comment":null,"returns_set":true,"input_arg_types":[{"schema" : "pg_catalog", "name" : "text", "type" : "b"}],"input_arg_names":["search"],"default_args":0,"function_oid":16697}]',
      ],
    ],
  },
  schema_list: {
    result_type: 'TuplesOk',
    result: [
      ['schema_name'],
      ['public'],
      ['authors_schema'],
      ['books_schema'],
      ['export'],
    ],
  },
};

export const allNetworkData = {
  postgresNetworkData,
  mssqlNetworkData,
};
