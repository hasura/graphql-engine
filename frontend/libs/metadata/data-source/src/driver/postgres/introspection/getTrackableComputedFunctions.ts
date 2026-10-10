import { RunSQLResponse } from '@hasura/shared/types';
import { GetTrackableFunctionProps } from '../../types';
import { runSQL } from '@hasura/metadata/api';
import { ArgType, TrackableComputedFunction } from '../types';

const functionDefinitionSql = `
SELECT p.proname::text AS function_name,
  pn.nspname::text AS function_schema,
  pg_get_functiondef(p.oid) AS function_definition,
  rtn.nspname::text AS return_type_schema,
  rt.typname::text AS return_type_name,
  rt.typtype::text AS return_type_type,
  p.proretset AS returns_set,
  (SELECT COALESCE(json_agg(json_build_object('schema', q.schema, 'name', q.name, 'type', q.type)), '[]'::json) AS "coalesce"
    FROM ( SELECT pt.typname AS name,
            pns.nspname AS schema,
            pt.typtype AS type,
            pat.ordinality
            FROM unnest(COALESCE(p.proallargtypes, p.proargtypes::oid[])) WITH ORDINALITY pat(oid, ordinality)
              LEFT JOIN pg_type pt ON pt.oid = pat.oid
              LEFT JOIN pg_namespace pns ON pt.typnamespace = pns.oid
          ORDER BY pat.ordinality) q) AS input_arg_types,
  to_json(COALESCE(p.proargnames, ARRAY[]::text[])) AS input_arg_names
FROM pg_proc p
  JOIN pg_namespace pn ON pn.oid = p.pronamespace
  JOIN pg_type rt ON rt.oid = p.prorettype
  JOIN pg_namespace rtn ON rtn.oid = rt.typnamespace
WHERE
  pn.nspname::text !~~ 'pg_%'::text
  AND p.provolatile::text = 's'::character(1)::text
  AND (pn.nspname::text <> ALL (ARRAY ['information_schema'::text, 'hdb_catalog', 'hdb_views', '_timescaledb_internal', 'crdb_internal', 'pg_catalog']))
  AND NOT (EXISTS (
    SELECT
      1 FROM pg_aggregate
    WHERE
      pg_aggregate.aggfnoid::oid = p.oid))
ORDER BY function_name ASC;
`;

const adaptIntrospectedFunctions = (
  sqlResponse: RunSQLResponse,
): TrackableComputedFunction[] => {
  return (sqlResponse.result ?? []).slice(1).map((row) => ({
    function: { name: row[0], schema: row[1] },
    definition: row[2],
    return_type: {
      name: row[4],
      schema: row[3],
    },
    return_type_type: row[5] as ArgType,
    returns_set: row[6] === 't',
    input_arg_types: JSON.parse(row[7]),
    input_arg_names: JSON.parse(row[8]),
  }));
};

export const getTrackableComputedFunctions = async ({
  dataSourceName,
  endpoints,
  fetchJson,
}: GetTrackableFunctionProps) => {
  const sqlResult = await runSQL({
    args: {
      source: {
        name: dataSourceName,
        kind: 'postgres',
      },
      sql: functionDefinitionSql,
      readOnly: true,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  return adaptIntrospectedFunctions(sqlResult);
};
