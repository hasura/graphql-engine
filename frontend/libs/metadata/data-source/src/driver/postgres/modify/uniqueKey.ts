import { PostgresFamilyDriver } from '@hasura/shared/types';
import { CreateUniqueKeyProps, DropUniqueKeyProps } from '../../types';
import { PostgresTable } from '../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import { getCreateUniqueKeySql, getDropConstraintSql } from '../sqlQueries';

export const createUniqueKeyCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: CreateUniqueKeyProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `add_unique_${schema}_${name}_${args.constraintName}`,
        up: [{ sql: getCreateUniqueKeySql(args) }],
        down: [
          {
            sql: getDropConstraintSql({
              table: args.table,
              constraintName: args.constraintName,
            }),
          },
        ],
      },
    }).then(() => true);
  };
};

export const dropUniqueKeyCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: DropUniqueKeyProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `drop_unique_${schema}_${name}_${args.constraintName}`,
        up: [
          {
            sql: getDropConstraintSql({
              table: args.table,
              constraintName: args.constraintName,
            }),
          },
        ],
        down: args.columns
          ? [
              {
                sql: getCreateUniqueKeySql({
                  table: args.table,
                  constraintName: args.constraintName,
                  columns: args.columns,
                }),
              },
            ]
          : [{ sql: '' }],
      },
    }).then(() => true);
  };
};
