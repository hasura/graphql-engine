import { GetViewDefinitionProps, GetViewDefinitionResult } from '../../types';
import { PostgresTable } from '../types';
import { runSQL } from '@hasura/metadata/api';
import { PostgresFamilyDriver, RunSQLResponse } from '@hasura/shared/types';

export const getViewDefinitionSql = (table: PostgresTable) => `
  SELECT
    CASE WHEN pg_has_role(c.relowner, 'USAGE') THEN pg_get_viewdef(c.oid)
    ELSE null
    END AS view_definition
  FROM pg_class c
  JOIN pg_namespace s on c.relnamespace = s.oid
  WHERE c.relname = '${table.name}'
    AND s.nspname = '${table.schema}' 
    AND c.relkind in ('v', 'm')
    AND (pg_has_role(c.relowner, 'USAGE')
    OR has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
    OR has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
  )
`;

const adaptViewDefinitions = (
  sqlResponse: RunSQLResponse,
): GetViewDefinitionResult[] => {
  return (sqlResponse.result ?? []).slice(1).map((row) => ({
    definition: row[0],
  }));
};

export const getPostgresViewDefinitionCurry =
  (driver: PostgresFamilyDriver) =>
  async ({
    endpoints,
    fetchJson,
    table,
    dataSourceName,
  }: GetViewDefinitionProps): Promise<GetViewDefinitionResult> => {
    const sql = getViewDefinitionSql(table as PostgresTable);

    const sqlResult = await runSQL({
      args: {
        source: {
          name: dataSourceName,
          kind: driver,
        },
        sql,
        readOnly: true,
      },
      fetchJson,
      url: endpoints.queryV2,
    });

    return adaptViewDefinitions(sqlResult)[0];
  };

export const getPostgresViewDefinition =
  getPostgresViewDefinitionCurry('postgres');
