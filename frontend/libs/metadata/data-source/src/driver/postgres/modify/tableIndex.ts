import { PostgresFamilyDriver } from '@hasura/shared/types';
import { CreateIndexProps, DropIndexProps } from '../../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import { getCreateIndexSql, getDropIndexSql } from '../sqlQueries';
import { PostgresTable } from '../types';

export const createIndexCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: CreateIndexProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `create_index_${schema}_${name}_${args.indexName}`,
        up: [{ sql: getCreateIndexSql(args) }],
        down: [
          {
            sql: getDropIndexSql({
              table: args.table,
              indexName: args.indexName,
            }),
          },
        ],
      },
    }).then(() => true);
  };
};

export const dropIndexCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: DropIndexProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `drop_index_${schema}_${name}_${args.indexName}`,
        up: [
          {
            sql: getDropIndexSql({
              table: args.table,
              indexName: args.indexName,
            }),
          },
        ],
        // No reliable down without the original CREATE INDEX definition.
        down: [{ sql: '' }],
      },
    }).then(() => true);
  };
};
