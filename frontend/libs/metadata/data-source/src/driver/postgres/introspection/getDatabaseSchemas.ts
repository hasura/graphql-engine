import { runSQL } from '@hasura/metadata/api';
import { GetDatabaseSchemaProps } from '../../types';
import { PostgresFamilyDriver } from '@hasura/shared/types';

const schemaListQuery = `
-- test_id = schema_list
SELECT schema_name FROM information_schema.schemata
WHERE
	schema_name NOT in('information_schema', 'pg_catalog', 'hdb_catalog', '_timescaledb_internal', 'pg_extension', 'crdb_internal')
	AND schema_name NOT LIKE 'pg_toast%'
	AND schema_name NOT LIKE 'pg_temp_%';
`;

export const getDatabaseSchemasCurry =
  (driver: PostgresFamilyDriver) =>
  async ({ dataSourceName, endpoints, fetchJson }: GetDatabaseSchemaProps) => {
    const response = await runSQL({
      url: endpoints.queryV2,
      fetchJson,
      args: {
        source: { name: dataSourceName, kind: driver },
        sql: schemaListQuery,
        readOnly: true,
      },
    });

    const schemas = response.result?.flat() ?? [];

    // remove first array item as that's the column header
    const [, ...result] = schemas;

    return result;
  };
