import { useMetadataSource } from '../../../MetadataAPI';

export const sourcesSupportingStreaming = ['postgres', 'mssql'];

export const useSourceSupportStreaming = (databaseName: string) => {
  const { data: sourceMetadata } = useMetadataSource(databaseName);
  const kind = sourceMetadata?.kind;

  if (!kind) return false;

  return sourcesSupportingStreaming.includes(kind);
};
