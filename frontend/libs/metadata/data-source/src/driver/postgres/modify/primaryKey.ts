import { PostgresFamilyDriver } from '@hasura/shared/types';
import {
  AlterPrimaryKeyProps,
  CreatePrimaryKeyProps,
  DropPrimaryKeyProps,
} from '../../types';
import { PostgresTable } from '../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import {
  getAlterPrimaryKeySql,
  getCreatePrimaryKeySql,
  getDropConstraintSql,
} from '../sqlQueries';

export const createPrimaryKeyCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: CreatePrimaryKeyProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `add_pk_${schema}_${name}`,
        up: [{ sql: getCreatePrimaryKeySql(args) }],
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

export const alterPrimaryKeyCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    previousColumns,
    ...args
  }: AlterPrimaryKeyProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `alter_pk_${schema}_${name}`,
        up: [{ sql: getAlterPrimaryKeySql(args) }],
        down: previousColumns
          ? [
              {
                sql: getAlterPrimaryKeySql({
                  table: args.table,
                  constraintName: args.constraintName,
                  columns: previousColumns,
                }),
              },
            ]
          : [{ sql: '' }],
      },
    }).then(() => true);
  };
};

export const dropPrimaryKeyCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: DropPrimaryKeyProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `drop_pk_${schema}_${name}`,
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
                sql: getCreatePrimaryKeySql({
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
