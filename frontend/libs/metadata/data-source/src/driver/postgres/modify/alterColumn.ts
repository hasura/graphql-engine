import { PostgresFamilyDriver } from '@hasura/shared/types';
import {
  AlterColumnArgs,
  AlterColumnDefinition,
  AlterColumnProps,
} from '../../types';
import { PostgresTable } from '../types';
import { runDatabaseMigration } from '@hasura/metadata/api';

const normalizeDefault = (value: AlterColumnDefinition['default']) =>
  value?.trim() ?? '';

/**
 * Builds the up/down SQL for altering a single column: type, default,
 * nullability, single-column uniqueness and name (mirrors the legacy
 * `getColumnUpdateMigration`). Every statement before the rename addresses the
 * column by its current name, so the rename always runs last on the way up and
 * first on the way down. Returns empty strings when nothing changed.
 */
export const getAlterColumnSql = ({
  table,
  previous,
  next,
}: AlterColumnArgs) => {
  const { schema, name: tableName } = table as PostgresTable;
  const qualifiedTable = `"${schema}"."${tableName}"`;
  const alterColumn = `ALTER TABLE ${qualifiedTable} ALTER COLUMN "${previous.name}"`;

  // Pairs of [up, down]; down statements are applied in reverse order.
  const steps: [string, string][] = [];

  if (next.type.trim() && next.type.trim() !== previous.type) {
    const type = next.type.trim();
    steps.push([
      `${alterColumn} TYPE ${type} USING "${previous.name}"::${type};`,
      `${alterColumn} TYPE ${previous.type} USING "${previous.name}"::${previous.type};`,
    ]);
  }

  const previousDefault = normalizeDefault(previous.default);
  const nextDefault = normalizeDefault(next.default);
  if (previousDefault !== nextDefault) {
    steps.push([
      nextDefault
        ? `${alterColumn} SET DEFAULT ${nextDefault};`
        : `${alterColumn} DROP DEFAULT;`,
      previousDefault
        ? `${alterColumn} SET DEFAULT ${previousDefault};`
        : `${alterColumn} DROP DEFAULT;`,
    ]);
  }

  if (previous.nullable !== next.nullable) {
    steps.push(
      next.nullable
        ? [`${alterColumn} DROP NOT NULL;`, `${alterColumn} SET NOT NULL;`]
        : [`${alterColumn} SET NOT NULL;`, `${alterColumn} DROP NOT NULL;`],
    );
  }

  if (previous.unique !== next.unique) {
    const constraintName =
      previous.uniqueConstraintName ?? `${tableName}_${previous.name}_key`;
    const addUnique = `ALTER TABLE ${qualifiedTable} ADD CONSTRAINT "${constraintName}" UNIQUE ("${previous.name}");`;
    const dropUnique = `ALTER TABLE ${qualifiedTable} DROP CONSTRAINT "${constraintName}";`;
    steps.push(next.unique ? [addUnique, dropUnique] : [dropUnique, addUnique]);
  }

  const newName = next.name.trim();
  if (newName && newName !== previous.name) {
    steps.push([
      `ALTER TABLE ${qualifiedTable} RENAME COLUMN "${previous.name}" TO "${newName}";`,
      `ALTER TABLE ${qualifiedTable} RENAME COLUMN "${newName}" TO "${previous.name}";`,
    ]);
  }

  return {
    up: steps.map(([up]) => up).join('\n'),
    down: steps
      .map(([, down]) => down)
      .reverse()
      .join('\n'),
  };
};

export const alterColumnCurry = (kind: PostgresFamilyDriver) => {
  return async ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: AlterColumnProps) => {
    const { up, down } = getAlterColumnSql(args);
    if (!up) return false;

    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `alter_table_${schema}_${name}_alter_column_${args.previous.name}`,
        up: [{ sql: up }],
        down: [{ sql: down }],
      },
    }).then(() => true);
  };
};
