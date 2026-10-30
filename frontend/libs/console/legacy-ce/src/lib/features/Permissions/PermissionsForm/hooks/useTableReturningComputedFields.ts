import { useQuery } from '@tanstack/react-query';
import { runSQL } from '@hasura/metadata/api';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { FetchJson } from '@hasura/shared/utils';
import { useAppContext } from '@hasura/shared/context';
import {
  ComputedField,
  PostgresComputedField,
  PostgresFamilyDriver,
  POSTGRESQL_FAMILY_DRIVERS,
  QualifiedDataSource,
} from '@hasura/shared/types';

const TABLE_RETURNING_COMPUTED_FIELDS_QUERY_KEY =
  'TABLE_RETURNING_COMPUTED_FIELDS';

const isPostgresFamily = (kind: string): kind is PostgresFamilyDriver =>
  (POSTGRESQL_FAMILY_DRIVERS as readonly string[]).includes(kind);

const quote = (value: string) => `'${value.replace(/'/g, "''")}'`;

// Mirrors the server's `returns_table` check (pg_function_metadata.sql): the
// function's return type is the row type of a table, view or materialized
// view.
const getTableReturningFunctionsSql = (
  functions: { schema: string; name: string }[],
) => `
SELECT pn.nspname::text AS function_schema, p.proname::text AS function_name
FROM pg_proc p
  JOIN pg_namespace pn ON pn.oid = p.pronamespace
  JOIN pg_type rt ON rt.oid = p.prorettype
  JOIN pg_class rc ON rc.oid = rt.typrelid
WHERE rc.relkind IN ('r', 'p', 'v', 'm', 'f')
  AND (pn.nspname::text, p.proname::text) IN (${functions
    .map(({ schema, name }) => `(${quote(schema)}, ${quote(name)})`)
    .join(', ')});`;

const getComputedFieldFunctions = (computedFields: ComputedField[]) =>
  (computedFields as PostgresComputedField[]).flatMap(
    ({ name, definition }) => {
      const func = definition?.function as
        { schema?: string; name?: string } | undefined;
      return func?.name
        ? [{ field: name, schema: func.schema ?? 'public', name: func.name }]
        : [];
    },
  );

export const canHaveTableReturningComputedFields = (
  source: Partial<QualifiedDataSource> | undefined,
  computedFields: ComputedField[],
): source is QualifiedDataSource =>
  Boolean(source?.name) &&
  isPostgresFamily(source?.kind ?? '') &&
  getComputedFieldFunctions(computedFields).length > 0;

export const getTableReturningComputedFieldsQueryKey = (
  source: Partial<QualifiedDataSource> | undefined,
  computedFields: ComputedField[],
) => [
  source?.name,
  TABLE_RETURNING_COMPUTED_FIELDS_QUERY_KEY,
  getComputedFieldFunctions(computedFields),
];

/**
 * Returns the names of the computed fields whose function returns a table row
 * (or a set of them). The server derives their select permissions from the
 * returning table, so they can't be picked in a select permission's
 * `computed_fields` nor in the row permissions builder.
 */
export const getTableReturningComputedFields = async ({
  source,
  computedFields,
  endpoints,
  fetchJson,
}: {
  source: QualifiedDataSource;
  computedFields: ComputedField[];
  endpoints: { queryV2: string };
  fetchJson: FetchJson;
}) => {
  const functions = getComputedFieldFunctions(computedFields);
  const sqlResult = await runSQL({
    args: {
      source: { name: source.name, kind: source.kind as PostgresFamilyDriver },
      sql: getTableReturningFunctionsSql(functions),
      readOnly: true,
    },
    fetchJson,
    url: endpoints.queryV2,
  });

  const tableReturning = (sqlResult.result ?? [])
    .slice(1)
    .map(([schema, name]) => `${schema}.${name}`);

  return functions
    .filter(({ schema, name }) => tableReturning.includes(`${schema}.${name}`))
    .map(({ field }) => field);
};

export const useTableReturningComputedFields = ({
  source,
  computedFields,
}: {
  source: Partial<QualifiedDataSource> | undefined;
  computedFields: ComputedField[];
}) => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  return useQuery({
    queryKey: getTableReturningComputedFieldsQueryKey(source, computedFields),
    queryFn: () =>
      getTableReturningComputedFields({
        source: source as QualifiedDataSource,
        computedFields,
        endpoints,
        fetchJson,
      }),
    enabled: canHaveTableReturningComputedFields(source, computedFields),
  });
};
