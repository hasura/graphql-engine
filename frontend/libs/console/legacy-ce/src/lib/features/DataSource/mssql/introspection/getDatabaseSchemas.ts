import { runSQL } from '../../api';
import { GetDatabaseSchemaProps } from '../../types';

const schemaListQuery = `
  -- test_id = schema_list
  SELECT
    s.name AS schema_name
  FROM
    sys.schemas s
  WHERE
    s.name NOT IN (
      'guest', 'INFORMATION_SCHEMA', 'sys',
      'db_owner', 'db_securityadmin', 'db_accessadmin',
      'db_backupoperator', 'db_ddladmin', 'db_datawriter',
      'db_datareader', 'db_denydatawriter', 'db_denydatareader', 'hdb_catalog'
    )
  ORDER BY
    s.name
`;

export const getDatabaseSchemas = async ({
  dataSourceName,
  httpClient,
}: GetDatabaseSchemaProps) => {
  const response = await runSQL({
    source: { name: dataSourceName, kind: 'mssql' },
    sql: schemaListQuery,
    httpClient,
    readOnly: true,
  });

  const schemas = response.result?.flat() ?? [];

  // remove first array item as that's the column header
  const [, ...rest] = schemas;

  return rest;
};
