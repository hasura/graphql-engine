import { exportMetadata } from '@/features/DataSource';
import { useMetadataMigration } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { useFireNotification } from '@/new-components/Notifications';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';

export const useUpdateComment = (dataSourceName: string, table: unknown) => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fireNotification } = useFireNotification();
  const httpClient = useHttpClient();
  const queryClient = useQueryClient();

  const updateComment = useCallback(
    async (comment: string) => {
      const { metadata, resource_version } = await exportMetadata({
        httpClient,
      });

      if (!metadata) throw Error('Unable to fetch metadata');

      const metadataSource = metadata.sources.find(
        s => s.name === dataSourceName
      );

      if (!metadataSource) throw Error('Unable to fetch metadata source');

      const driver = metadataSource.kind;

      mutate(
        {
          query: {
            resource_version,
            type: `${driver}_set_table_customization`,
            args: {
              source: dataSourceName,
              table,
              configuration: {
                comment,
              },
            },
          },
        },
        {
          onSuccess: () => {
            queryClient.invalidateQueries(['manage-table', dataSourceName]);
            fireNotification({
              type: 'success',
              title: 'Success!',
              message: 'Comment saved!',
            });
          },
          onError: err => {
            fireNotification({
              type: 'error',
              title: 'Failed to save comment.',
              message: err?.message,
            });
          },
        }
      );
    },
    [dataSourceName, fireNotification, httpClient, mutate, queryClient, table]
  );

  return { updateComment, ...rest };
};
