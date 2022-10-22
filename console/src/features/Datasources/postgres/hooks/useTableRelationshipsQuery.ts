import { useRunSQL } from '../../common';
import { DataTarget, TUseTableRelationshipsQuery } from '../../drivers';
import { RunSQLQueryOptions } from '../../types';
import { isPostgresDataTarget } from '../types';

const getSQL = (tableName: string) => {
  return `SELECT conrelid::regclass AS "source_table"
    ,CASE WHEN pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %' THEN substring(pg_get_constraintdef(c.oid), 14, position(')' in pg_get_constraintdef(c.oid))-14) END AS "source_column"
    ,CASE WHEN pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %' THEN substring(pg_get_constraintdef(c.oid), position(' REFERENCES ' in pg_get_constraintdef(c.oid))+12, position('(' in substring(pg_get_constraintdef(c.oid), 14))-position(' REFERENCES ' in pg_get_constraintdef(c.oid))+1) END AS "target_table"
    ,CASE WHEN pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %' THEN substring(pg_get_constraintdef(c.oid), position('(' in substring(pg_get_constraintdef(c.oid), 14))+14, position(')' in substring(pg_get_constraintdef(c.oid), position('(' in substring(pg_get_constraintdef(c.oid), 14))+14))-1) END AS "target_column"
    FROM   pg_constraint c
    JOIN   pg_namespace n ON n.oid = c.connamespace
    WHERE  contype IN ('f', 'p') 
    AND (conrelid::regclass = '"${tableName}"'::regclass OR substring(pg_get_constraintdef(c.oid), position(' REFERENCES ' in pg_get_constraintdef(c.oid))+12, position('(' in substring(pg_get_constraintdef(c.oid), 14))-position(' REFERENCES ' in pg_get_constraintdef(c.oid))+1) = '"${tableName}"')
    AND pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %';
  `;
};

export const useTableRelationshipsQuery: TUseTableRelationshipsQuery = ({
  target,
  queryOptions,
}: {
  target: DataTarget;
  queryOptions?: RunSQLQueryOptions<string[], any>;
}) => {
  if (!isPostgresDataTarget(target))
    throw Error('Not a valid postgres data path');

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
    queryKey: ['postgres', 'fk_relationships', database, schema, table],
    transformFn: data => {
      const { result } = data;

      if (!result) throw Error(`Invalid SQL response: received ${result}`);

      if (!result.length)
        throw Error(
          `Invalid SQL Response: recieved 0 rows, received ${result}`
        );

      try {
        const rows = result.slice(1).map(row => ({
          from: {
            table: row[0],
            column: row[1].split(','),
          },
          to: {
            table: row[2],
            column: row[3].split(','),
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
      driver: 'postgres',
    },
  });

  return query;
};
