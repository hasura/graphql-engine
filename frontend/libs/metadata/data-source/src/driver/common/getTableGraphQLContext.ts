import { buildClientSchema, GraphQLSchema } from 'graphql';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { exportMetadata, runIntrospectionQuery } from '@hasura/metadata/api';
import { Table } from '@hasura/shared/types';
import { DataSourceNetworkArgs } from '../types';

/**
 * The GraphQL schema only changes along with the metadata resource version
 * (schema-changing `run_sql` bumps it too), so it is introspected once per
 * version instead of on every request.
 */
let cachedSchema: { key: string; schema: Promise<GraphQLSchema> } | undefined;

const getGraphQLSchema = ({
  endpoints,
  fetchJson,
  resourceVersion,
}: DataSourceNetworkArgs & { resourceVersion: number }) => {
  const key = `${endpoints.graphQLUrl}#${resourceVersion}`;
  if (cachedSchema?.key === key) return cachedSchema.schema;

  const schema = runIntrospectionQuery({
    url: endpoints.graphQLUrl,
    fetchJson,
  }).then(buildClientSchema);
  cachedSchema = { key, schema };

  // don't keep a failed introspection around
  schema.catch(() => {
    if (cachedSchema?.schema === schema) cachedSchema = undefined;
  });

  return schema;
};

/**
 * Returns what's needed to build GraphQL operations on a tracked table: the
 * live GraphQL schema and the table and source customizations.
 */
export const getTableGraphQLContext = async ({
  dataSourceName,
  table,
  endpoints,
  fetchJson,
  caller,
}: DataSourceNetworkArgs & {
  dataSourceName: string;
  table: Table;
  /** prefixes the error messages */
  caller: string;
}) => {
  const { resource_version, metadata } = await exportMetadata({
    fetchJson,
    url: endpoints.metadata,
  });

  const source = metadata.sources.find(({ name }) => name === dataSourceName);
  if (!source) throw new Error(`${caller}: source not found in metadata`);

  const trackedTable = source.tables.find((t) =>
    areTablesEqual(t.table, table),
  );
  if (!trackedTable)
    throw new Error(`${caller}: trackedTable not found in metadata`);

  return {
    schema: await getGraphQLSchema({
      endpoints,
      fetchJson,
      resourceVersion: resource_version,
    }),
    tableCustomization: trackedTable.configuration,
    sourceCustomization: source.customization,
  };
};
