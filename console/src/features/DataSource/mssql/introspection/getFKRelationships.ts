import { MssqlTable } from '..';
import { runSQL, RunSQLResponse } from '../../api';
import { GetFKRelationshipProps, TableFkRelationships } from '../../types';

const adaptFkRelationships = (
  result: RunSQLResponse['result']
): TableFkRelationships[] => {
  if (!result) return [];

  return result.slice(1).map(row => ({
    from: {
      table: { name: row[0], schema: row[1] },
      column: JSON.parse(row[4]).map(
        (col: { column: string; referenced_column: string }) => col.column
      ),
    },
    to: {
      table: { name: row[2], schema: row[3] },
      column: JSON.parse(row[4]).map(
        (col: { column: string; referenced_column: string }) =>
          col.referenced_column
      ),
    },
  }));
};

export const getFKRelationships = async ({
  dataSourceName,
  table,
  httpClient,
}: GetFKRelationshipProps) => {
  const { name } = table as MssqlTable;

  const sql = `SELECT
  tab1.name AS [fromTable],
  sch1.name AS [fromSchema],
  tab2.name AS [toTable],
  sch2.name AS [toSchema],
    (
        SELECT
            col1.name AS [column],
            col2.name AS [referenced_column]
        FROM sys.foreign_key_columns fkc
        INNER JOIN sys.columns col1
            ON col1.column_id = fkc.parent_column_id AND col1.object_id = tab1.object_id
        INNER JOIN sys.columns col2
            ON col2.column_id = fkc.referenced_column_id AND col2.object_id = tab2.object_id
        WHERE fk.object_id = fkc.constraint_object_id
        FOR JSON PATH
    ) AS column_mapping
FROM sys.foreign_keys fk
INNER JOIN sys.objects obj
    ON obj.object_id = fk.referenced_object_id
INNER JOIN sys.tables tab1
    ON tab1.object_id = fk.parent_object_id
INNER JOIN sys.schemas sch1
    ON tab1.schema_id = sch1.schema_id
INNER JOIN sys.tables tab2
    ON tab2.object_id = fk.referenced_object_id
INNER JOIN sys.schemas sch2
    ON tab2.schema_id = sch2.schema_id
WHERE tab1.name = '${name}' OR tab2.name = '${name}'`;

  const response = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'mssql',
    },
    sql,
    httpClient,
  });

  return adaptFkRelationships(response.result);
};
