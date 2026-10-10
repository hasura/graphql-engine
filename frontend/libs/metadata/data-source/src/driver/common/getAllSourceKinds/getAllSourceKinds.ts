import { SupportedDriver } from '@hasura/shared/types';
import { runMetadataQuery } from '@hasura/metadata/api';
import { DataSourceNetworkArgs } from '../../types';

type SourceKindsResponse = {
  sources: {
    builtin: boolean;
    kind: SupportedDriver;
    display_name: string;
    release_name?: string;
    available: boolean;
  }[];
};
export const getAllSourceKinds = async ({
  fetchJson,
  endpoints,
}: DataSourceNetworkArgs) => {
  const result = await runMetadataQuery<SourceKindsResponse>({
    body: {
      type: 'list_source_kinds',
      args: {},
    },
    fetchJson,
    url: endpoints.metadata,
  });

  // Allow GDC sources and non-MySQL native sources
  return result.sources.filter((source) => {
    const isGDCSource = source.builtin === false;
    const nonMySQLNativeSource =
      source.kind !== 'mysql' && source.builtin === true;
    return isGDCSource || nonMySQLNativeSource;
  });
};
