import {
  AllowedRunSQLKeys,
  BulkQueryType,
  NativeDriver,
  QualifiedDataSource,
  RunSQLResponse,
} from '@hasura/shared/types';
import { NetworkArgs } from '../types';

export type RunSqlArgs = {
  // The database on which the sql is to be executed (default: 'default' database)
  source: QualifiedDataSource;
  sql: string;
  // When set to true, the effect (if possible) is cascaded to any hasuradb dependent objects (relationships, permissions, templates).
  cascade?: boolean;
  // When set to true, the request will be run in READ ONLY transaction access mode which means only select queries will be successful.
  // This flag ensures that the GraphQL schema is not modified and is hence highly performant.
  readOnly?: boolean;
  // When set to false, the sql is executed without checking Metadata dependencies.
  checkMetadataConsistency?: boolean;
  // When set to true, statements are executed outside transaction blocks.
  // Useful for operations like CREATE INDEX CONCURRENTLY that cannot run within transactions (default: false)
  noTransaction?: boolean;
};

// https://hasura.io/docs/2.0/api-reference/schema-api/run-sql/#schema-run-sql-syntax
export type GetRunSqlQueryReturn = {
  type: AllowedRunSQLKeys;
  args: {
    source: string;
    sql: string;
    cascade?: boolean;
    read_only?: boolean;
    check_metadata_consistency?: boolean;
    no_transaction?: boolean;
  };
};

const getRunSqlType = (driver: NativeDriver) => {
  const prefix = driver === 'postgres' || driver === 'alloy' ? 'pg' : driver;
  return `${prefix}_run_sql` as const;
};

export function getRunSqlQuery({
  sql,
  source,
  cascade,
  readOnly,
  checkMetadataConsistency,
  noTransaction,
}: RunSqlArgs): GetRunSqlQueryReturn {
  return {
    type: getRunSqlType(source.kind as NativeDriver),
    args: {
      source: source.name,
      sql,
      cascade,
      read_only: readOnly,
      check_metadata_consistency: checkMetadataConsistency,
      no_transaction: noTransaction,
    },
  };
}

export const runSQL = async ({
  url,
  fetchJson,
  args,
}: NetworkArgs & {
  args: RunSqlArgs;
}): Promise<RunSQLResponse> => {
  return fetchJson(url, {
    method: 'POST',
    body: JSON.stringify(getRunSqlQuery(args)),
  });
};

export type RunSQLBulkProps = {
  // `runSQLBulk` targets the `/v2/query` endpoint, which supports `bulk` and
  // `concurrent_bulk` (not the metadata-only `bulk_atomic`/`bulk_keep_going`).
  type?: BulkQueryType;
  args: RunSqlArgs[];
};

export const runSQLBulk = async ({
  url,
  fetchJson,
  args,
  type = 'bulk',
}: RunSQLBulkProps & NetworkArgs): Promise<RunSQLResponse[]> => {
  return fetchJson(url, {
    method: 'POST',
    body: JSON.stringify({
      type,
      args: args.map(getRunSqlQuery),
    }),
  });
};
