import { CitusTable } from '..';
import { runSQL, RunSQLResponse } from '../../api';
import { GetFKRelationshipProps, TableFkRelationships } from '../../types';

const getTable = (tableName: string) => {
  const splitResult = tableName.split('.');
  if (splitResult.length === 1)
    return { schema: 'public', name: splitResult[0] };
  return { schema: splitResult[0], name: splitResult[1] };
};

const adaptFkRelationships = (
  result: RunSQLResponse['result']
): TableFkRelationships[] => {
  if (!result) return [];
  const adaptedResult: TableFkRelationships[] = result.slice(1).map(row => {
    const sourceTable: CitusTable = getTable(row[0]);
    const targetTable: CitusTable = getTable(row[2]);

    return {
      from: {
        /**
         * This is to remove the schema name from tables that are not from `public` schema
         */
        table: sourceTable,
        /**
         * break complex fk joins into array of string and remove and `"` character in the names
         */
        column: row[1].split(',')?.map(i => i?.replace(/"/g, '')),
      },
      to: {
        table: targetTable,
        column: row[3].split(',')?.map(i => i?.replace(/"/g, '')),
      },
    };
  });

  return adaptedResult;
};

export const getFKRelationships = async ({
  dataSourceName,
  table,
  httpClient,
}: GetFKRelationshipProps) => {
  const { schema, name } = table as CitusTable;
  /**
   * This SQL goes through the pg_constraint (https://www.postgresql.org/docs/current/catalog-pg-constraint.html) and the pg_namespace (https://www.postgresql.org/docs/14/catalog-pg-namespace.html)
   * and links fk_constraint object that have the same Object ID on both tables and finally
   * delivers the list of all possible FKConstraints for a given table.
   * The contype checks basically checks for - f = foreign key constraint, p = primary key constraint,
   */
  const sql = `SELECT conrelid::regclass AS "source_table"
  ,CASE WHEN pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %' THEN substring(pg_get_constraintdef(c.oid), 14, position(')' in pg_get_constraintdef(c.oid))-14) END AS "source_column"
  ,CASE WHEN pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %' THEN substring(pg_get_constraintdef(c.oid), position(' REFERENCES ' in pg_get_constraintdef(c.oid))+12, position('(' in substring(pg_get_constraintdef(c.oid), 14))-position(' REFERENCES ' in pg_get_constraintdef(c.oid))+1) END AS "target_table"
  ,CASE WHEN pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %' THEN substring(pg_get_constraintdef(c.oid), position('(' in substring(pg_get_constraintdef(c.oid), 14))+14, position(')' in substring(pg_get_constraintdef(c.oid), position('(' in substring(pg_get_constraintdef(c.oid), 14))+14))-1) END AS "target_column"
  FROM   pg_constraint c
  JOIN   pg_namespace n ON n.oid = c.connamespace
  WHERE  contype IN ('f', 'p') 
  AND (conrelid::regclass = '"${schema}"."${name}"'::regclass OR substring(pg_get_constraintdef(c.oid), position(' REFERENCES ' in pg_get_constraintdef(c.oid))+12, position('(' in substring(pg_get_constraintdef(c.oid), 14))-position(' REFERENCES ' in pg_get_constraintdef(c.oid))+1) = '${
    schema === 'public' ? `"${name}"` : `${schema}.${name}`
  }')
  AND pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %';
`;

  const response = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'citus',
    },
    sql,
    httpClient,
  });

  return adaptFkRelationships(response.result);
};
