import {
  MetadataTableConfig,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';
import { manageTableMetadataQueryKey, useMetadataTable } from '.';

export const useUpdateTableConfiguration = (
  dataSourceName: string,
  table: unknown
) => {
  const { mutate, ...rest } = useMetadataMigration();

  const { fireNotification } = useFireNotification();

  const queryClient = useQueryClient();

  const { metadata, resource_version, metadataTable } = useMetadataTable(
    dataSourceName,
    table
  );

  const updateTableConfiguration = useCallback(
    (config: MetadataTableConfig) => {
      const driver = metadata?.kind;

      return new Promise<void>((resolve, reject) => {
        if (!metadata) {
          throw Error('Metadata not found!');
        }

        mutate(
          {
            query: {
              resource_version,
              type: `${driver}_set_table_customization`,
              args: {
                source: dataSourceName,
                table,
                configuration: Object.assign(
                  metadataTable?.configuration || {},
                  config
                ),
              },
            },
          },
          {
            onSuccess: () => {
              queryClient.invalidateQueries(
                manageTableMetadataQueryKey(dataSourceName)
              );
              fireNotification({
                type: 'success',
                title: 'Success!',
                message: 'Configuration saved!',
              });
              resolve();
            },
            onError: err => {
              fireNotification({
                type: 'error',
                title: 'Failed to save configuration.',
                message: err?.message,
              });
              reject();
            },
          }
        );
      });
    },
    [
      dataSourceName,
      fireNotification,
      metadata,
      mutate,
      queryClient,
      resource_version,
      table,
    ]
  );

  return { updateTableConfiguration, metadata, resource_version, ...rest };
};
