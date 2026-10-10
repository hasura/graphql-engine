import { PostgresTable } from '../types';
import { runSQL } from '@hasura/metadata/api';
import {
  GetFKRelationshipProps,
  TableFkRelationships,
  ViolationAction,
} from '../../types';
import { adaptStringForPostgres } from '../utils';
import { PostgresFamilyDriver, RunSQLResponse } from '@hasura/shared/types';
import trim from 'lodash/trim';

const getTable = (tableName: string) => {
  const splitResult = tableName.split('.');
  if (splitResult.length === 1)
    return { schema: 'public', name: trim(splitResult[0], '"') };

  return {
    schema: trim(splitResult[0], '"'),
    name: trim(splitResult[1], '"'),
  };
};

const adaptFkRelationships = (
  result: RunSQLResponse['result'],
): TableFkRelationships[] => {
  if (!result) return [];
  const adaptedResult: TableFkRelationships[] = result.slice(1).map((row) => {
    const sourceTable: PostgresTable = getTable(row[0]);
    const targetTable: PostgresTable = getTable(row[2]);

    return {
      from: {
        /**
         * This is to remove the schema name from tables that are not from `public` schema
         */
        table: sourceTable,
        /**
         * break complex fk joins into array of string and remove and `"` character in the names
         */
        columns: row[1].split(',')?.map((i) => trim(i, '"')),
      },
      to: {
        table: targetTable,
        columns: row[3].split(',')?.map((i) => trim(i, '"')),
      },
      onUpdate: adaptReferenceOption(row[4]),
      onDelete: adaptReferenceOption(row[5]),
      name: row[6],
    };
  });

  return adaptedResult;
};

const adaptReferenceOption = (opt: string): ViolationAction => {
  switch (opt) {
    case 'a':
      return 'no action';
    case 'c':
      return 'cascade';
    case 'n':
      return 'set null';
    case 'd':
      return 'set default';
    case 'r':
    default:
      return 'restrict';
  }
};

export const getFKRelationshipsCurry =
  (kind: PostgresFamilyDriver) =>
  async ({
    dataSourceName,
    table,
    fetchJson,
    endpoints,
  }: GetFKRelationshipProps) => {
    const { schema, name } = table as PostgresTable;

    const qualifiedSchemaName = adaptStringForPostgres(schema);
    const qualifiedTableName = adaptStringForPostgres(name);

    const qualifiedTable =
      schema === 'public'
        ? qualifiedTableName
        : `${qualifiedSchemaName}.${qualifiedTableName}`;
    /**
     * This SQL goes through the pg_constraint (https://www.postgresql.org/docs/current/catalog-pg-constraint.html) and the pg_namespace (https://www.postgresql.org/docs/14/catalog-pg-namespace.html)
     * and links fk_constraint object that have the same Object ID on both tables and finally
     * delivers the list of all possible FKConstraints for a given table.
     * The contype checks basically checks for - f = foreign key constraint, p = primary key constraint,
     */
    const sql = `SELECT conrelid::regclass AS "source_table"
  , CASE WHEN pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %' THEN substring(pg_get_constraintdef(c.oid), 14, position(')' in pg_get_constraintdef(c.oid))-14) END AS "source_column"
  , CASE WHEN pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %' THEN substring(pg_get_constraintdef(c.oid), position(' REFERENCES ' in pg_get_constraintdef(c.oid))+12, position('(' in substring(pg_get_constraintdef(c.oid), 14))-position(' REFERENCES ' in pg_get_constraintdef(c.oid))+1) END AS "target_table"
  , CASE WHEN pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %' THEN substring(pg_get_constraintdef(c.oid), position('(' in substring(pg_get_constraintdef(c.oid), 14))+14, position(')' in substring(pg_get_constraintdef(c.oid), position('(' in substring(pg_get_constraintdef(c.oid), 14))+14))-1) END AS "target_column"
  , c.confupdtype
  , c.confdeltype
  , c.conname
  FROM   pg_constraint c
  JOIN   pg_namespace n ON n.oid = c.connamespace
  WHERE  contype IN ('f', 'p') 
  AND (conrelid::regclass = '${qualifiedTable}'::regclass OR substring(pg_get_constraintdef(c.oid), position(' REFERENCES ' in pg_get_constraintdef(c.oid))+12, position('(' in substring(pg_get_constraintdef(c.oid), 14))-position(' REFERENCES ' in pg_get_constraintdef(c.oid))+1) = '${qualifiedTable}')
  AND pg_get_constraintdef(c.oid) LIKE 'FOREIGN KEY %';
`;

    const response = await runSQL({
      args: {
        source: {
          name: dataSourceName,
          kind,
        },
        sql,
        readOnly: true,
      },
      fetchJson,
      url: endpoints.queryV2,
    });

    return adaptFkRelationships(response.result);
  };

export const getFKRelationships = getFKRelationshipsCurry('postgres');
