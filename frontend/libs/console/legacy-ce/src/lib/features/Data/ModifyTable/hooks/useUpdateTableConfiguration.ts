import { useCallback } from 'react';
import { useFireNotification } from '../../../../new-components/Notifications';
import { useMetadataMigration } from '../../../MetadataAPI';
import { MetadataUtils, useMetadata } from '../../../hasura-metadata-api';
import { MetadataTable } from '../../../hasura-metadata-types';

export const useUpdateTableConfiguration = (
  dataSourceName: string,
  table: unknown
) => {
  const { mutate, ...rest } = useMetadataMigration();

  const { fireNotification } = useFireNotification();

  const { data } = useMetadata(m => ({
    source: MetadataUtils.findMetadataSource(dataSourceName, m),
    resource_version: m.resource_version,
    metadataTable: MetadataUtils.findMetadataTable(dataSourceName, table, m),
  }));

  const { source, metadataTable, resource_version } = data || {};

  const updateTableConfiguration = useCallback(
    (config: MetadataTable['configuration']) => {
      const driver = source?.kind;

      return new Promise<void>((resolve, reject) => {
        if (!source) {
          throw Error('Data Source not found!');
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
      mutate,
      metadataTable,
      resource_version,
      source,
      table,
    ]
  );

  // helper function
  const updateCustomRootFields = useCallback(
    (config: MetadataTable['configuration']) => {
      const newConfig: MetadataTable['configuration'] = {
        ...metadataTable?.configuration,
        custom_name: config?.custom_name || undefined,
        custom_root_fields: config?.custom_root_fields || {},
      };

      return updateTableConfiguration(newConfig);
    },
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [updateTableConfiguration]
  );

  return {
    updateTableConfiguration,
    updateCustomRootFields,
    source,
    resource_version,
    ...rest,
  };
};
