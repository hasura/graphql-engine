import { PostgresFamilyDriver } from '@hasura/shared/types';
import {
  CreateTriggerArgs,
  CreateTriggerProps,
  DropTriggerProps,
} from '../../types';
import { runDatabaseMigration } from '@hasura/metadata/api';
import { getDropTriggerSql } from '../sqlQueries';
import { PostgresTable } from '../types';

export const dropTriggerCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: DropTriggerProps) => {
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `drop_trigger_${schema}_${name}_${args.triggerName}`,
        up: [
          {
            sql: getDropTriggerSql({
              table: args.table,
              triggerName: args.triggerName,
            }),
          },
        ],
        // No reliable down without the original CREATE TRIGGER definition.
        down: [{ sql: '' }],
      },
    }).then(() => true);
  };
};

/**
 * Builds the up/down SQL for `CREATE TRIGGER`, optionally creating a new
 * plpgsql trigger function first (dropped again on the way down).
 * CockroachDB only accepts `EXECUTE FUNCTION`; Postgres accepts `EXECUTE
 * PROCEDURE` on every version (`EXECUTE FUNCTION` needs 11+).
 */
export const getCreateTriggerSql = (
  kind: PostgresFamilyDriver,
  {
    table,
    triggerName,
    timing,
    events,
    forEach,
    condition,
    function: fn,
    newFunctionBody,
  }: CreateTriggerArgs,
) => {
  const { schema, name } = table as PostgresTable;
  const qualifiedFunction = `"${fn.schema}"."${fn.name}"`;
  const executeKeyword = kind === 'cockroach' ? 'FUNCTION' : 'PROCEDURE';

  const up: string[] = [];
  const down: string[] = [];

  if (newFunctionBody !== undefined) {
    up.push(`CREATE FUNCTION ${qualifiedFunction}()
RETURNS TRIGGER
LANGUAGE plpgsql
AS $$
${newFunctionBody.trim()}
$$;`);
    down.push(`DROP FUNCTION ${qualifiedFunction}();`);
  }

  const when = condition?.trim() ? `\nWHEN (${condition.trim()})` : '';
  up.push(`CREATE TRIGGER "${triggerName}"
${timing} ${events.join(' OR ')} ON "${schema}"."${name}"
FOR EACH ${forEach}${when}
EXECUTE ${executeKeyword} ${qualifiedFunction}();`);
  down.unshift(getDropTriggerSql({ table, triggerName }));

  return { up: up.join('\n'), down: down.join('\n') };
};

export const createTriggerCurry = (kind: PostgresFamilyDriver) => {
  return ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    ...args
  }: CreateTriggerProps) => {
    const { up, down } = getCreateTriggerSql(kind, args);
    const source = { name: dataSourceName, kind };
    const { schema, name } = args.table as PostgresTable;
    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `create_trigger_${schema}_${name}_${args.triggerName}`,
        up: [{ sql: up }],
        down: [{ sql: down }],
      },
    }).then(() => true);
  };
};
