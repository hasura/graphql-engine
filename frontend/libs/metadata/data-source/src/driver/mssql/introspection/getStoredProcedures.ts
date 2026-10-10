import { runSQL } from '@hasura/metadata/api';
import { GetStoredProceduresProps } from '../../types';
import { QualifiedFunction, RunSQLResponse } from '@hasura/shared/types';

const adaptGetStoredProcedures = (
  result: RunSQLResponse['result'],
): QualifiedFunction[] => {
  return (
    result?.slice(1).map((row) => ({
      name: row[0],
      schema: row[1],
    })) ?? []
  );
};

export const getStoredProcedures = async ({
  dataSourceName,
  endpoints,
  fetchJson,
}: GetStoredProceduresProps) => {
  const sql = `
  select routine_name, routine_schema
  from information_schema.routines 
 where routine_type = 'PROCEDURE'
  `;

  const result = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'mssql',
      },
      sql: sql,
      readOnly: true,
    },
    url: endpoints.queryV2,
    fetchJson,
  });

  return adaptGetStoredProcedures(result.result);
};
