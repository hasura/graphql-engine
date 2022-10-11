import { NetworkArgs, runMetadataQuery } from '../../api';

type SourceKindsResponse = {
  sources: { builtin: boolean; kind: string }[];
};
export const getAllSourceKinds = async ({ httpClient }: NetworkArgs) => {
  const result = await runMetadataQuery<SourceKindsResponse>({
    httpClient,
    body: {
      type: 'list_source_kinds',
      args: {},
    },
  });

  return result.sources;
};
