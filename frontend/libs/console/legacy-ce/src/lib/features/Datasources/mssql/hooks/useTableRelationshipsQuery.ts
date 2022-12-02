import { useRunSQL } from '../../common';
import { DataTarget, TUseTableRelationshipsQuery } from '../../drivers';
import { RunSQLQueryOptions } from '../../types';
import { isMSSQLDataTarget } from '../types';

const getSQL = (tableName: string) => {
  return `SELECT
  tab1.name AS [table_name],
  tab2.name AS [ref_table],
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
WHERE tab1.name = '${tableName}' OR tab2.name = '${tableName}'`;
};

export const useTableRelationshipsQuery: TUseTableRelationshipsQuery = ({
  target,
  queryOptions,
}: {
  target: DataTarget;
  queryOptions?: RunSQLQueryOptions<string[], any>;
}) => {
  if (!isMSSQLDataTarget(target)) throw Error('Not a valid MSSQL data path');

  const { database, schema, table } = target;

  const query = useRunSQL<
    string[],
    {
      from: {
        table: string;
        column: string[];
      };
      to: {
        table: string;
        column: string[];
      };
    }[]
  >({
    sql: getSQL(table),
    queryKey: ['mssql', 'fk_relationships', database, schema, table],
    transformFn: data => {
      const { result } = data;

      if (!result) throw Error(`Invalid SQL response: recieved ${result}`);

      if (!result.length)
        throw Error(
          `Invalid SQL Response: recieved 0 rows, received ${result}`
        );

      try {
        const rows = result.slice(1).map(row => ({
          from: {
            table: row[0],
            column: JSON.parse(row[2]).map(
              (col: { column: string; referenced_column: string }) => col.column
            ),
          },
          to: {
            table: row[1],
            column: JSON.parse(row[2]).map(
              (col: { column: string; referenced_column: string }) =>
                col.referenced_column
            ),
          },
        }));

        return rows;
      } catch (err) {
        throw Error('Unable to parse response');
      }
    },
    queryOptions,
    dataSource: {
      name: database,
      driver: 'mssql',
    },
  });

  return query;
};
