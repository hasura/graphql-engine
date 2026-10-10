import { RunSQLResponse } from '@hasura/shared/types';
import { GetTrackableFunctionProps, IntrospectedFunction } from '../../types';
import { runSQL } from '@hasura/metadata/api';

export const adaptIntrospectedFunctions = (
  sqlResponse: RunSQLResponse,
): IntrospectedFunction[] => {
  return (sqlResponse.result ?? []).slice(1).map((row) => ({
    name: row[0],
    function: { name: row[0], schema: row[1] },
    isVolatile: row[2] === 'VOLATILE',
  }));
};

export const FUNCTION_TYPE_COLUMN_SQL = `    CASE
      WHEN pgp.provolatile::text = 'i'::character(1)::text THEN 'IMMUTABLE'::text
      WHEN pgp.provolatile::text = 's'::character(1)::text THEN 'STABLE'::text
      WHEN pgp.provolatile::text = 'v'::character(1)::text THEN 'VOLATILE'::text
      ELSE NULL::text
    END AS function_type`;

export const getTrackableFunctions = async ({
  dataSourceName,
  endpoints,
  fetchJson,
}: GetTrackableFunctionProps) => {
  const sql = `
  SELECT 
    pgp.proname AS function_name, 
    pn.nspname AS schema,
${FUNCTION_TYPE_COLUMN_SQL} 
  FROM 
    pg_proc pgp 
  JOIN pg_namespace pn ON pgp.pronamespace = pn.oid 
  JOIN pg_type ON pgp.prorettype = pg_type.oid
  WHERE 
    pg_type.typtype = 'c' AND
    pn.nspname NOT IN ('information_schema') AND pn.nspname NOT LIKE 'pg_%';
  `;

  const sqlResult = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'postgres',
      },
      sql,
      readOnly: true,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  return adaptIntrospectedFunctions(sqlResult);
};
