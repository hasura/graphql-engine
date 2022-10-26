import {
  MetadataTableConfig,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { useFireNotification } from '@/new-components/Notifications';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';
import { useMetadataForManageTable, manageTableMetadataQueryKey } from '.';

export const useUpdateTableConfiguration = (
  dataSourceName: string,
  table: unknown
) => {
  const { mutate, ...rest } = useMetadataMigration();

  const { fireNotification } = useFireNotification();

  const queryClient = useQueryClient();

  const { data } = useMetadataForManageTable(dataSourceName);

  const metadata = data?.metadata;

  const resource_version = data?.resource_version;

  const updateTableConfiguration = useCallback(
    (config: MetadataTableConfig) => {
      const driver = metadata?.kind;
      return new Promise<void>((resolve, reject) => {
        mutate(
          {
            query: {
              resource_version,
              type: `${driver}_set_table_customization`,
              args: {
                source: dataSourceName,
                table,
                configuration: config,
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

  return { updateTableConfiguration, ...rest };
};
