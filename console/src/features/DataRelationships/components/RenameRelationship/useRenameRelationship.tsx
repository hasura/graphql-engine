import { exportMetadata } from '@/features/DataSource';
import { Table, useMetadataMigration } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { useFireNotification } from '@/new-components/Notifications';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';

type EditManualLocalRelationshipPayload = {
  newName: string;
  relationshipName: string;
  fromTable: Table;
  fromSource: string;
};

export const useRenameRelationship = () => {
  const httpClient = useHttpClient();
  const { fireNotification } = useFireNotification();
  const { mutate, ...rest } = useMetadataMigration();
  const queryClient = useQueryClient();

  const renameRelationship = useCallback(
    async ({
      values,
      onSuccess,
      onError,
    }: {
      values: EditManualLocalRelationshipPayload;
      onSuccess?: () => void;
      onError?: (err: unknown) => void;
    }) => {
      try {
        const { resource_version, metadata } = await exportMetadata({
          httpClient,
        });

        if (!metadata) throw Error('Unable to fetch metadata');

        const metadataSource = metadata.sources.find(
          s => s.name === values.fromSource
        );

        if (!metadataSource) throw Error('Unable to fetch metadata source');

        const driver = metadataSource.kind;

        const type = 'rename_relationship';

        mutate(
          {
            query: {
              resource_version,
              type: `${driver}_${type}`,
              args: {
                table: values.fromTable,
                source: values.fromSource,
                name: values.relationshipName,
                new_name: values.newName,
              },
            },
          },
          {
            onSuccess: () => {
              queryClient.refetchQueries([
                values.fromSource,
                'list_all_relationships',
              ]);

              onSuccess?.();

              fireNotification({
                type: 'success',
                title: 'Success!',
                message: 'A relationship renamed succesfully!',
              });
            },
            onError: err => {
              onError?.(err);
              fireNotification({
                type: 'error',
                title: 'Failed to rename relationship',
                message: err?.message,
              });
            },
          }
        );
      } catch (err) {
        fireNotification({
          title: 'Error',
          type: 'error',
          message: JSON.stringify(err),
        });
      }
    },
    [fireNotification, httpClient, mutate, queryClient]
  );

  return { renameRelationship, ...rest };
};
