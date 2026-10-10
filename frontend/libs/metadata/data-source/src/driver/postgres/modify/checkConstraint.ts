import { PostgresFamilyDriver } from '@hasura/shared/types';
import {
  CreateCheckConstraintProps,
  DropCheckConstraintProps,
} from '../../types';
import { PostgresTable } from '../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import {
  getCreateCheckConstraintSql,
  getDropConstraintSql,
} from '../sqlQueries';

export const createCheckConstraintCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: CreateCheckConstraintProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `add_check_constraint_${schema}_${name}_${args.constraintName}`,
        up: [{ sql: getCreateCheckConstraintSql(args) }],
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

export const dropCheckConstraintCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: DropCheckConstraintProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `drop_check_constraint_${schema}_${name}_${args.constraintName}`,
        up: [
          {
            sql: getDropConstraintSql({
              table: args.table,
              constraintName: args.constraintName,
            }),
          },
        ],
        // Re-add on rollback only when the CHECK expression is known.
        down: args.check
          ? [
              {
                sql: getCreateCheckConstraintSql({
                  table: args.table,
                  constraintName: args.constraintName,
                  check: args.check,
                }),
              },
            ]
          : [{ sql: '' }],
      },
    }).then(() => true);
  };
};
