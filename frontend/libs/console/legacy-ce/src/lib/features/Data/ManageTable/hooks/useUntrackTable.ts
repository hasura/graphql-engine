import { useCallback } from 'react';
import {
  useInvalidateMetadata,
  useMetadata,
} from '../../../hasura-metadata-api';
import { Table } from '../../../hasura-metadata-types';
import { useMetadataMigration } from '../../../MetadataAPI';

export const useUntrackTable = (props?: {
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) => {
  const { mutate, ...rest } = useMetadataMigration();
  const invalidateMetadata = useInvalidateMetadata();

  const { onSuccess, onError } = props ?? {};

  const { data: metadataSources } = useMetadata(m => m.metadata.sources);

  const untrackTable = useCallback(
    ({ dataSourceName, table }: { dataSourceName: string; table: Table }) => {
      const driver = metadataSources?.find(
        source => source.name === dataSourceName
      )?.kind;

      if (!driver) throw Error('Unable to find source in metadata');

      mutate(
        {
          query: {
            type: `${driver}_untrack_table`,
            args: {
              table,
              source: dataSourceName,
            },
          },
        },
        {
          onSuccess: () => {
            invalidateMetadata();
            onSuccess?.();
          },
          onError: err => {
            onError?.(err);
          },
        }
      );
    },
    [metadataSources, mutate, onSuccess, onError]
  );

  return { untrackTable, ...rest };
};
