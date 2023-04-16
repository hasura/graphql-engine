import { NetworkArgs, runMetadataQuery } from '../../api';

type SourceKindsResponse = {
  sources: {
    builtin: boolean;
    kind: string;
    display_name: string;
    release_name?: string;
    available: boolean;
  }[];
};
export const getAllSourceKinds = async ({ httpClient }: NetworkArgs) => {
  const result = await runMetadataQuery<SourceKindsResponse>({
    httpClient,
    body: {
      type: 'list_source_kinds',
      args: {},
    },
  });

  // Allow GDC sources and non-MySQL native sources
  return result.sources.filter(source => {
    const isGDCSource = source.builtin === false;
    const nonMySQLNativeSource =
      source.kind !== 'mysql' && source.builtin === true;
    return isGDCSource || nonMySQLNativeSource;
  });
};
