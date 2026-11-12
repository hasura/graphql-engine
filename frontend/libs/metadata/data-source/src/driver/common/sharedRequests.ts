import { buildClientSchema, GraphQLSchema } from 'graphql';
import { exportMetadata, runIntrospectionQuery } from '@hasura/metadata/api';
import { Metadata } from '@hasura/shared/types';
import { DataSourceNetworkArgs } from '../types';

/**
 * The metadata export shared by the introspection calls running at the same
 * time (e.g. the columns of every table a permission builder loads), so they
 * make one request. It is dropped once settled, so it is never stale.
 */
let pendingMetadata: { url: string; metadata: Promise<Metadata> } | undefined;

export const exportMetadataShared = ({
  endpoints,
  fetchJson,
}: DataSourceNetworkArgs): Promise<Metadata> => {
  const url = endpoints.metadata;
  if (pendingMetadata?.url === url) return pendingMetadata.metadata;

  const metadata = exportMetadata({ url, fetchJson });
  pendingMetadata = { url, metadata };

  const clear = () => {
    if (pendingMetadata?.metadata === metadata) pendingMetadata = undefined;
  };
  metadata.then(clear, clear);

  return metadata;
};

/**
 * The GraphQL schema only changes along with the metadata resource version
 * (schema-changing `run_sql` bumps it too), so it is introspected once per
 * version instead of on every request.
 */
let cachedSchema: { key: string; schema: Promise<GraphQLSchema> } | undefined;

export const getGraphQLSchema = ({
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
