import { PostgresFamilyDriver } from '@hasura/shared/types';
import { CreateForeignKeyProps, ModifyForeignKeyArgs } from '../../types';
import { PostgresTable } from '../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import { getDropConstraintSql } from '../sqlQueries';
import { sanitizeMigrationName } from '../../utils';

export const getCreateForeignKeySql = ({
  from,
  to,
  constraintName,
  onUpdate,
  onDelete,
}: ModifyForeignKeyArgs) => {
  const formTable = from.table as PostgresTable;
  const toTable = to.table as PostgresTable;

  return `
  alter table "${formTable.schema}"."${formTable.name}"
  add constraint "${constraintName}"
  foreign key (${from.columns.map((column) => `"${column}"`).join(', ')})
  references "${toTable.schema}"."${toTable.name}"
  (${to.columns.map((column) => `"${column}"`).join(', ')}) on update ${onUpdate} on delete ${onDelete};
`;
};

export const createForeignKeyCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: CreateForeignKeyProps) => {
    const source = { name: dataSourceName, kind };
    const { name, schema } = args.from.table as PostgresTable;
    const constraintName =
      args.constraintName ||
      `add_constraint_${sanitizeMigrationName(
        schema,
        name,
        ...args.from.columns,
      )}_fkey`;

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `set_fk_${schema}_${name}_${args.from.columns.join('_')}`,
        up: [
          {
            sql: getCreateForeignKeySql({
              ...args,
              constraintName,
            }),
          },
        ],
        down: [
          {
            sql: getDropConstraintSql({
              table: args.from.table,
              constraintName: constraintName,
            }),
          },
        ],
      },
    }).then(() => true);
  };
};
