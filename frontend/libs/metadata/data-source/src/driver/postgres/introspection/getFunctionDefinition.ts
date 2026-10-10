import {
  GetFunctionDefinitionProps,
  GetFunctionDefinitionResult,
} from '../../types';
import { PostgresFunction } from '../types';
import { runSQL } from '@hasura/metadata/api';
import { PostgresFamilyDriver, RunSQLResponse } from '@hasura/shared/types';
import { FUNCTION_TYPE_COLUMN_SQL } from './getTrackableFunctions';

const adaptFunctionDefinitions = (
  sqlResponse: RunSQLResponse,
): GetFunctionDefinitionResult[] => {
  return (sqlResponse.result ?? []).slice(1).map((row) => ({
    definition: row[0],
    isVolatile: row[1] === 'VOLATILE',
    returnTable:
      row[2] && row[3]
        ? {
            schema: row[2],
            name: row[3],
          }
        : undefined,
  }));
};

export const getPostgresFunctionDefinitionCurry =
  (driver: PostgresFamilyDriver) =>
  async ({
    endpoints,
    fetchJson,
    func,
    dataSourceName,
  }: GetFunctionDefinitionProps): Promise<GetFunctionDefinitionResult> => {
    const { schema, name } = func as PostgresFunction;

    const sql = `
SELECT 
  pg_get_functiondef(pgp.oid) AS function_definition,
${FUNCTION_TYPE_COLUMN_SQL},
  rtn.nspname::text AS return_type_schema,
  rt.typname::text AS return_type_name
FROM 
  pg_proc pgp 
JOIN pg_namespace pn ON pgp.pronamespace = pn.oid 
JOIN pg_type rt ON rt.oid = pgp.prorettype
JOIN pg_namespace rtn ON rtn.oid = rt.typnamespace
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

    return adaptFunctionDefinitions(sqlResult)[0];
  };

export const getPostgresFunctionDefinition =
  getPostgresFunctionDefinitionCurry('postgres');
