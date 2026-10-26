import { areTablesEqual } from '@hasura/metadata/helpers';
import { Metadata, Table } from '@hasura/shared/types';
import { DataSourceNetworkArgs } from '../types';
import { exportMetadataShared, getGraphQLSchema } from './sharedRequests';

/**
 * Returns what's needed to build GraphQL operations on a tracked table: the
 * live GraphQL schema and the table and source customizations. The metadata
 * is exported unless it is passed in.
 */
export const getTableGraphQLContext = async ({
  dataSourceName,
  table,
  endpoints,
  fetchJson,
  caller,
  metadata: exportedMetadata,
}: DataSourceNetworkArgs & {
  dataSourceName: string;
  table: Table;
  /** prefixes the error messages */
  caller: string;
  metadata?: Metadata;
}) => {
  const { resource_version, metadata } =
    exportedMetadata ?? (await exportMetadataShared({ endpoints, fetchJson }));

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
