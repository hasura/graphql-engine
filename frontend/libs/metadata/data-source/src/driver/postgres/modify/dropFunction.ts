import { runDatabaseMigration, runSQL } from '@hasura/metadata/api';
import { DropFunctionProps } from '../../types';
import { PostgresFamilyDriver } from '@hasura/shared/types';
import { PostgresFunction } from '../types';

const getFunctionDefinitionAndArguments =
  (driver: PostgresFamilyDriver) =>
  async ({
    endpoints,
    fetchJson,
    func,
    dataSourceName,
  }: Omit<DropFunctionProps, 'isMigration'>) => {
    const { schema, name } = func as PostgresFunction;

    const sql = `
SELECT 
  pg_get_functiondef(pgp.oid) AS function_definition,
  pg_catalog.pg_get_function_identity_arguments(pgp.oid)
FROM 
  pg_proc pgp 
JOIN pg_namespace pn ON pgp.pronamespace = pn.oid 
WHERE pgp.proname = '${name}' AND pn.nspname = '${schema}' LIMIT 1;`;

    const sqlResult = await runSQL({
      args: {
        source: {
          name: dataSourceName,
          kind: driver,
        },
        sql,
        readOnly: true,
      },
      fetchJson,
      url: endpoints.queryV2,
    });

    return (sqlResult.result ?? []).slice(1).map((row) => ({
      definition: row[0],
      arguments: row[1],
    }));
  };

export const dropFunctionCurry = (kind: PostgresFamilyDriver) => {
  return async ({
    dataSourceName,
    endpoints,
    fetchJson,
    isMigration,
    func,
  }: DropFunctionProps) => {
    const { name, schema } = func as PostgresFunction;
    const source = { name: dataSourceName, kind };
    const definitions = await getFunctionDefinitionAndArguments(kind)({
      dataSourceName,
      endpoints,
      fetchJson,
      func,
    });

    if (!definitions.length) {
      return false;
    }

    const upSQL = definitions
      .map((def) => `DROP FUNCTION "${schema}"."${name}"(${def.arguments});`)
      .join('\n');

    return runDatabaseMigration({
      endpoints,
      fetchJson,
      isMigration,
      args: {
        source,
        name: `drop_function_${schema}_${name}`,
        up: [
          {
            sql: upSQL,
          },
        ],
        down: [
          {
            sql: definitions[0].definition,
          },
        ],
      },
    }).then((result) => Boolean(result?.length));
  };
};
