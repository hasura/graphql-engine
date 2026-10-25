import { useCallback } from 'react';
import { useMetadata } from './useMetadata';

export const useMetadataHelpers = () => {
  const { refetch } = useMetadata(undefined, {
    enabled: false,
  });

  const fetchMetadata = useCallback(async () => {
    // join a fetch already in flight rather than cancelling it, so callers
    // fetching at the same time share one request
    const metadataResult = await refetch({ cancelRefetch: false });
    if (metadataResult.error) {
      throw metadataResult.error;
    }

    if (!metadataResult.data) {
      throw new Error('Failed to fetch metadata');
    }

    return metadataResult.data;
  }, [refetch]);

  const fetchSource = useCallback(
    async (sourceName: string) => {
      const meta = await fetchMetadata();
      const source = meta.metadata.sources.find((s) => s.name === sourceName);
      if (!source) {
        throw new Error(`Source ${sourceName} does not exist`);
      }

      return {
        ...meta,
        source,
      };
    },
    [fetchMetadata],
  );

  return {
    fetchMetadata,
    fetchSource,
  };
};
