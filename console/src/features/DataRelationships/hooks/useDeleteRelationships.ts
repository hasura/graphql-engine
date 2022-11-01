import { exportMetadata } from '@/features/DataSource';
import { Table, useMetadataMigration } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { useFireNotification } from '@/new-components/Notifications';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';
import { Relationship } from '../components/DatabaseRelationshipsTable/types';

export const useDeleteRelationship = (props?: { onSuccess?: () => void }) => {
  const { mutate, ...rest } = useMetadataMigration();
  const httpClient = useHttpClient();
  const { fireNotification } = useFireNotification();
  const queryClient = useQueryClient();

  const deleteRelationship = useCallback(
    async (dataSource: string, table: Table, relationship: Relationship) => {
      const { metadata, resource_version } = await exportMetadata({
        httpClient,
      });

      if (!metadata) throw Error('Unable to fetch metadata');

      const metadataSource = metadata.sources.find(s => s.name === dataSource);

      if (!metadataSource) throw Error('Unable to fetch metadata source');

      const driver = metadataSource.kind;

      const isRelationshipRemote =
        relationship.type === 'toSource' ||
        relationship.type === 'toRemoteSchema';
      const type = isRelationshipRemote
        ? 'delete_remote_relationship'
        : 'drop_relationship';

      mutate(
        {
          query: {
            resource_version,
            type: `${driver}_${type}`,
            args: {
              table,
              source: dataSource,
              [isRelationshipRemote ? 'name' : 'relationship']:
                relationship.name,
            },
          },
        },
        {
          onSuccess: () => {
            queryClient.refetchQueries([dataSource, 'list_all_relationships']);

            props?.onSuccess?.();

            fireNotification({
              type: 'success',
              title: 'Success!',
              message: 'Relationship deleted successfully!',
            });
          },
          onError: err => {
            fireNotification({
              type: 'error',
              title: 'Failed to delete relationship',
              message: err?.message,
            });
          },
        }
      );
    },
    [fireNotification, httpClient, mutate, props, queryClient]
  );

  return {
    deleteRelationship,
    ...rest,
  };
};
