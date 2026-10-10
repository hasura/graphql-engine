import { runSQL } from '@hasura/metadata/api';
import { GetTableColumnsProps, TableColumn } from '../../types';
import { adaptSQLDataType } from '../../postgres/utils';
import { CockroachDBTable } from '../types';
import { RunSQLResponse } from '@hasura/shared/types';

const adaptTableColumns = (result: RunSQLResponse['result']): TableColumn[] => {
  if (!result) return [];

  return result.slice(1).map((row) => ({
    name: row[0],
    consoleDataType: adaptSQLDataType(row[1]),
    dataType: row[1],
    nullable: row[2] === 'YES',
    defaultValue: row[3] ?? null,
  }));
};

export const getTableColumns = async ({
  dataSourceName,
  table,
  endpoints,
  fetchJson,
}: GetTableColumnsProps) => {
  const { name, schema } = table as CockroachDBTable;

  const sql = `
  SELECT
   column_name, data_type, is_nullable, column_default
  FROM
    information_schema.columns
  WHERE
    table_schema = '${schema}' AND
    table_name  = '${name}';`;

  const tables = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'cockroach',
      },
      sql,
      readOnly: true,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  return adaptTableColumns(tables.result);
};
