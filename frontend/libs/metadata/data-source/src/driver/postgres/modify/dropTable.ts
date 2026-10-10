import { PostgresTable } from '../types';
import { PostgresFamilyDriver } from '@hasura/shared/types';
import { DropTableProps } from '../../types';
import { runDatabaseMigration } from '@hasura/metadata/api';

export const getDropTableOrViewSql = (
  table: PostgresTable,
  property: 'table' | 'view' = 'table',
  cascade?: boolean,
) =>
  `drop ${property} "${table.schema}"."${table.name}"${cascade ? ' CASCADE' : ''};`;

export const dropTableCurry = (kind: PostgresFamilyDriver) => {
  return async ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    table,
    cascade,
  }: DropTableProps) => {
    const source = { name: dataSourceName, kind };
    const pgTable = table as PostgresTable;

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `drop_table_${pgTable.schema}_${pgTable.name}`,
        up: [
          {
            sql: getDropTableOrViewSql(pgTable, 'table', cascade),
          },
        ],
        down: [],
      },
    }).then(() => true);
  };
};
