import { runSQL } from '@hasura/metadata/api';
import { ChangeDatabaseSchemaProps } from '../../types';

const getDropSchemaSql = (schema: string) => {
  return `drop schema ${schema};`;
};

export const deleteDatabaseSchema = async ({
  dataSourceName,
  schemaName,
  endpoints,
  fetchJson,
}: ChangeDatabaseSchemaProps) => {
  const response = await runSQL({
    args: {
      source: { name: dataSourceName, kind: 'mssql' },
      sql: getDropSchemaSql(schemaName),
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  return response;
};
