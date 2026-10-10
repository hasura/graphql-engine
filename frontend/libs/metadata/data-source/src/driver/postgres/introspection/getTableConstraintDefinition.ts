import { PostgresTable } from '../types';
import { runSQL } from '@hasura/metadata/api';
import { DataSourceNetworkArgs } from '../../types';
import { adaptStringForPostgres } from '../utils';
import { PostgresFamilyDriver, Table } from '@hasura/shared/types';

type GetTableConstraintDefinitionProps = DataSourceNetworkArgs & {
  dataSourceName: string;
  constraintName: string;
  table: Table;
  // c = check constraint, f = foreign key constraint, n = not-null constraint, p = primary key constraint, u = unique constraint, t = constraint trigger, x = exclusion constraint
  types: ('c' | 'f' | 'n' | 'p' | 'u' | 't' | 'x')[];
};

export const getTableConstraintDefinitionCurry =
  (kind: PostgresFamilyDriver) =>
  async ({
    dataSourceName,
    table,
    fetchJson,
    endpoints,
    constraintName,
    types,
  }: GetTableConstraintDefinitionProps): Promise<string | undefined> => {
    const { schema, name } = table as PostgresTable;

    const qualifiedSchemaName = adaptStringForPostgres(schema);
    const qualifiedTableName = adaptStringForPostgres(name);

    const qualifiedTable =
      schema === 'public'
        ? qualifiedTableName
        : `${qualifiedSchemaName}.${qualifiedTableName}`;
    const constraintType = types.map((t) => `'${t}'`).join(', ');
    /**
     * This SQL goes through the pg_constraint (https://www.postgresql.org/docs/current/catalog-pg-constraint.html) and the pg_namespace (https://www.postgresql.org/docs/14/catalog-pg-namespace.html)
     * and links fk_constraint object that have the same Object ID on both tables and finally
     * delivers the list of all possible FKConstraints for a given table.
     * The contype checks basically checks for - f = foreign key constraint, p = primary key constraint,
     */
    const sql = `SELECT pg_get_constraintdef(c.oid) AS "definition"
  FROM   pg_constraint c
  JOIN   pg_namespace n ON n.oid = c.connamespace
  WHERE  contype IN (${constraintType}) 
  AND (conrelid::regclass = '${qualifiedTable}'::regclass
  AND c.conname = '${constraintName}'
  LIMIT 1;
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

    return response.result && response.result.length > 1
      ? response.result[1][0]
      : undefined;
  };

export const getTableConstraintDefinition =
  getTableConstraintDefinitionCurry('postgres');
