import { QualifiedTable } from '../../../metadata/types';

const getTableClause = (ts: QualifiedTable[]) => (t: QualifiedTable) => {
  return ts
    .map(
      table => `(
        ${t.name} = '${table.name}'
          AND ${t.schema} = '${table.schema}'
        )`
    )
    .join(` OR `);
};

const getSchemaClause = (s: string[]) => (n: string) =>
  s.length ? `${n} in (${s.map(d => `'${d}'`).join(',')})` : '';

export const getSchemasWhereClause =
  (schemas: string[], clausePrefix = 'WHERE') =>
  (sqlSchemas: string[] | string): string => {
    const schemaClauses = !Array.isArray(sqlSchemas)
      ? getSchemaClause(schemas)(sqlSchemas)
      : sqlSchemas.map(getSchemaClause(schemas)).filter(Boolean).join(' OR ');
    return schemaClauses ? `${clausePrefix} (${schemaClauses})` : '';
  };

export const getTablesWhereClause =
  (tables: QualifiedTable[], clausePrefix = 'WHERE') =>
  (sqlTables: QualifiedTable[] | QualifiedTable): string => {
    const tableClauses = !Array.isArray(sqlTables)
      ? getTableClause(tables)(sqlTables)
      : sqlTables.map(getTableClause(tables)).filter(Boolean).join(' OR ');
    return tableClauses ? `${clausePrefix} (${tableClauses})` : '';
  };
