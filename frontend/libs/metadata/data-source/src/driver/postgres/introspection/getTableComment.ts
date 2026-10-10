import {
  DataSourceNetworkArgs,
  GetFunctionCommentProps,
  GetTableCommentProps,
  GetViewCommentProps,
} from '../../types';
import { PostgresFunction, PostgresTable } from '../types';
import { runSQL } from '@hasura/metadata/api';
import {
  PostgresFamilyDriver,
  QualifiedDataSource,
  RunSQLResponse,
} from '@hasura/shared/types';

// The relkinds of tables (ordinary, partitioned, foreign) and views
// (plain, materialized) in pg_class.
const TABLE_RELKINDS = `'r', 'p', 'f'`;
const VIEW_RELKINDS = `'v', 'm'`;

export const getRelationCommentSql = (
  { schema, name }: PostgresTable,
  relkinds: string,
) => `
SELECT obj_description(c.oid, 'pg_class') AS comment
FROM pg_class c
  JOIN pg_namespace s ON c.relnamespace = s.oid
WHERE c.relname = '${name}'
  AND s.nspname = '${schema}'
  AND c.relkind IN (${relkinds})
LIMIT 1;
`;

export const getFunctionCommentSql = ({ schema, name }: PostgresFunction) => `
SELECT obj_description(p.oid, 'pg_proc') AS comment
FROM pg_proc p
  JOIN pg_namespace s ON p.pronamespace = s.oid
WHERE p.proname = '${name}'
  AND s.nspname = '${schema}'
LIMIT 1;
`;

/** First column of the first data row; `undefined` when absent or NULL. */
const adaptComment = (sqlResponse: RunSQLResponse): string | undefined =>
  sqlResponse.result?.[1]?.[0] || undefined;

const runCommentQuery = async (
  source: QualifiedDataSource,
  sql: string,
  { endpoints, fetchJson }: DataSourceNetworkArgs,
) => {
  const sqlResult = await runSQL({
    args: { source, sql, readOnly: true },
    fetchJson,
    url: endpoints.queryV2,
  });
  return adaptComment(sqlResult);
};

export const getPostgresTableCommentCurry =
  (driver: PostgresFamilyDriver) =>
  ({ table, dataSourceName, ...network }: GetTableCommentProps) =>
    runCommentQuery(
      { name: dataSourceName, kind: driver },
      getRelationCommentSql(table as PostgresTable, TABLE_RELKINDS),
      network,
    );

export const getPostgresViewCommentCurry =
  (driver: PostgresFamilyDriver) =>
  ({ table, dataSourceName, ...network }: GetViewCommentProps) =>
    runCommentQuery(
      { name: dataSourceName, kind: driver },
      getRelationCommentSql(table as PostgresTable, VIEW_RELKINDS),
      network,
    );

export const getPostgresFunctionCommentCurry =
  (driver: PostgresFamilyDriver) =>
  ({ func, dataSourceName, ...network }: GetFunctionCommentProps) =>
    runCommentQuery(
      { name: dataSourceName, kind: driver },
      getFunctionCommentSql(func as PostgresFunction),
      network,
    );

export const getTableComment = getPostgresTableCommentCurry('postgres');
export const getViewComment = getPostgresViewCommentCurry('postgres');
export const getFunctionComment = getPostgresFunctionCommentCurry('postgres');
