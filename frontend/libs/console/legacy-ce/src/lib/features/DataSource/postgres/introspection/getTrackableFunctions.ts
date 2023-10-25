import { runSQL, RunSQLResponse } from '../../api';
import { GetTrackableFunctionProps, IntrospectedFunction } from '../../types';

const adaptIntrospectedFunctions = (
  sqlResponse: RunSQLResponse
): IntrospectedFunction[] => {
  return (sqlResponse.result ?? []).slice(1).map(row => ({
    name: row[0],
    qualifiedFunction: { name: row[0], schema: row[1] },
    isVolatile: row[2] === 'VOLATILE',
  }));
};

export const getTrackableFunctions = async ({
  dataSourceName,
  httpClient,
}: GetTrackableFunctionProps) => {
  const sql = `
  SELECT 
    pgp.proname AS function_name, 
    pn.nspname AS schema,
  CASE
        WHEN pgp.provolatile::text = 'i'::character(1)::text THEN 'IMMUTABLE'::text
        WHEN pgp.provolatile::text = 's'::character(1)::text THEN 'STABLE'::text
        WHEN pgp.provolatile::text = 'v'::character(1)::text THEN 'VOLATILE'::text
        ELSE NULL::text
    END AS function_type 
  FROM 
    pg_proc pgp 
  JOIN pg_namespace pn ON pgp.pronamespace = pn.oid 
  JOIN pg_type ON pgp.prorettype = pg_type.oid
  WHERE 
    pg_type.typtype = 'c' AND
    pn.nspname NOT IN ('information_schema') AND pn.nspname NOT LIKE 'pg_%';
  `;

  const sqlResult = await runSQL({
    source: {
      name: dataSourceName,
      kind: 'postgres',
    },
    sql,
    httpClient,
  });

  return adaptIntrospectedFunctions(sqlResult);
};
