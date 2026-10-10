import { runSQL } from '@hasura/metadata/api';
import { ChangeDatabaseSchemaProps } from '../../types';

const getCreateSchemaSql = (schema: string) => {
  return `create schema ${schema};`;
};

export const createDatabaseSchema = async ({
  dataSourceName,
  schemaName,
  fetchJson,
  endpoints,
}: ChangeDatabaseSchemaProps) => {
  const response = await runSQL({
    args: {
      source: { name: dataSourceName, kind: 'mssql' },
      sql: getCreateSchemaSql(schemaName),
    },
    fetchJson,
    url: endpoints.queryV2,
  });
  return response;
};
