import { PostgresFamilyDriver } from '@hasura/shared/types';
import { ModifyForeignKeyProps } from '../../types';
import { PostgresTable } from '../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import { getCreateForeignKeySql } from './createForeignKey';
import { getDropConstraintSql } from '../sqlQueries';

export const dropForeignKeyCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: ModifyForeignKeyProps) => {
    const source = { name: dataSourceName, kind };
    const { name, schema } = args.from.table as PostgresTable;

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `set_fk_${schema}_${name}_${args.from.columns.join('_')}`,
        up: [
          {
            sql: getDropConstraintSql({
              table: args.from.table,
              constraintName: args.constraintName,
            }),
          },
        ],
        down: [
          {
            sql: getCreateForeignKeySql(args),
          },
        ],
      },
    }).then(() => true);
  };
};
