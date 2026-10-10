import { PostgresTable } from '../types';
import { PostgresFamilyDriver } from '@hasura/shared/types';
import { ChangeTableNameProps } from '../../types';
import { runDatabaseMigration } from '@hasura/metadata/api';

const renameTableOrView = (
  identity: string,
  schemaName: string,
  oldName: string,
  newName: string,
) => `
alter ${identity.toLowerCase()} "${schemaName}"."${oldName}" rename to "${newName}";`;

export const changeTableNameCurry = (kind: PostgresFamilyDriver) => {
  return async ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    table,
    newName,
  }: ChangeTableNameProps) => {
    const source = { name: dataSourceName, kind };
    const pgTable = table.table as PostgresTable;
    const identity = table.type.toLowerCase();

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `rename_${identity}_${pgTable.schema}_${pgTable.name}`,
        up: [
          {
            sql: renameTableOrView(
              identity,
              pgTable.schema,
              pgTable.name,
              newName,
            ),
          },
        ],
        down: [
          {
            sql: renameTableOrView(
              identity,
              pgTable.schema,
              newName,
              pgTable.name,
            ),
          },
        ],
      },
    }).then(() => true);
  };
};
