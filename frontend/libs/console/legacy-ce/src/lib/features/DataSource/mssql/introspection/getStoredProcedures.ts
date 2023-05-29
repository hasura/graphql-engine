import { RunSQLResponse, runSQL } from '../../api';
import { GetStoredProceduresProps, StoredProcedure } from '../../types';

const adaptGetStoredProcedures = (
  result: RunSQLResponse['result']
): StoredProcedure[] => {
  return (
    result?.slice(1).map(row => ({
      name: row[0],
      schema: row[1],
    })) ?? []
  );
};

export const getStoredProcedures = async ({
  dataSourceName,
  httpClient,
}: GetStoredProceduresProps) => {
  const sql = `
  select routine_name, routine_schema
  from information_schema.routines 
 where routine_type = 'PROCEDURE'
  `;

  const result = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'mssql',
    },
    sql: sql,
    readOnly: true,
    httpClient,
  });

  return adaptGetStoredProcedures(result.result);
};
