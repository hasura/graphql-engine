import { PostgresFamilyDriver } from '@hasura/shared/types';
import { DropColumnArgs, DropColumnProps } from '../../types';
import { PostgresTable } from '../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import { getCreatePrimaryKeySql, getCreateUniqueKeySql } from '../sqlQueries';
import { getCreateForeignKeySql } from './createForeignKey';

/**
 * Builds the up/down SQL for dropping a column (mirrors the legacy
 * `deleteColumnSql`). The drop is not cascaded, so it fails while other
 * database objects (e.g. foreign keys from other tables) still depend on it.
 * The down migration restores the column's definition and the constraints of
 * this table that involved it — not its data.
 */
export const getDropColumnSql = ({
  table,
  column,
  primaryKey,
  uniqueKeys = [],
  foreignKeys = [],
}: DropColumnArgs) => {
  const { schema, name: tableName } = table as PostgresTable;
  const qualifiedTable = `"${schema}"."${tableName}"`;
  const columnDefault = column.default?.trim() ?? '';

  let columnDef = `"${column.name}" ${column.type}`;
  if (columnDefault) columnDef += ` DEFAULT ${columnDefault}`;
  // Without a default, existing rows would violate NOT NULL on re-add.
  if (!column.nullable && columnDefault) columnDef += ' NOT NULL';

  const down = [`ALTER TABLE ${qualifiedTable} ADD COLUMN ${columnDef};`];
  if (primaryKey) {
    down.push(getCreatePrimaryKeySql({ table, ...primaryKey }));
  }
  uniqueKeys.forEach((uk) =>
    down.push(getCreateUniqueKeySql({ table, ...uk })),
  );
  foreignKeys
    .filter((fk) => fk.name)
    .forEach((fk) =>
      down.push(
        getCreateForeignKeySql({
          constraintName: fk.name as string,
          from: fk.from,
          to: fk.to,
          onUpdate: fk.onUpdate ?? 'restrict',
          onDelete: fk.onDelete ?? 'restrict',
        }).trim(),
      ),
    );

  return {
    up: `ALTER TABLE ${qualifiedTable} DROP COLUMN "${column.name}";`,
    down: down.join('\n'),
  };
};

export const dropColumnCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: DropColumnProps) => {
    const { up, down } = getDropColumnSql(args);
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `alter_table_${schema}_${name}_drop_column_${args.column.name}`,
        up: [{ sql: up }],
        down: [{ sql: down }],
      },
    }).then(() => true);
  };
};
