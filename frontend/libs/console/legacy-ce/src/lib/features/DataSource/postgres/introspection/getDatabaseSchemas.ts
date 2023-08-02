import { runSQL } from '../../api';
import { GetDatabaseSchemaProps } from '../../types';

const schemaListQuery = `
-- test_id = schema_list
SELECT schema_name FROM information_schema.schemata
WHERE
	schema_name NOT in('information_schema', 'pg_catalog', 'hdb_catalog', '_timescaledb_internal')
	AND schema_name NOT LIKE 'pg_toast%'
	AND schema_name NOT LIKE 'pg_temp_%';
`;

export const getDatabaseSchemas = async ({
  dataSourceName,
  httpClient,
}: GetDatabaseSchemaProps) => {
  const response = await runSQL({
    source: { name: dataSourceName, kind: 'postgres' },
    sql: schemaListQuery,
    httpClient,
    readOnly: true,
  });

  const schemas = response.result?.flat() ?? [];

  // remove first array item as that's the column header
  const [, ...rest] = schemas;

  return rest;
};
