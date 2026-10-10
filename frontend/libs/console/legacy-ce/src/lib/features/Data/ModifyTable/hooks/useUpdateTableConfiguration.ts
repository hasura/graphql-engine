import { useCallback } from 'react';
import {
  useErrorNotification,
  useMetadataMigration,
  useMetadata,
} from '@hasura/metadata/api';
import { MetadataTable, Table } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import { getDriverPrefix, MetadataSelectors } from '@hasura/metadata/helpers';

export const useUpdateTableConfiguration = (
  dataSourceName: string,
  table: Table,
) => {
  const { mutateAsync, ...rest } = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const { data } = useMetadata((m) => ({
    source: MetadataSelectors.findMetadataSource(dataSourceName, m),
    resource_version: m.resource_version,
    metadataTable: MetadataSelectors.findMetadataTable(
      dataSourceName,
      table,
      m,
    ),
  }));

  const { source, metadataTable, resource_version } = data || {};

  const updateTableConfiguration = useCallback(
    (config: MetadataTable['configuration']) => {
      if (!source?.kind) {
        throw Error('Data Source not found!');
      }

      return mutateAsync(
        {
          query: {
            resource_version,
            type: `${getDriverPrefix(source.kind)}_set_table_customization`,
            args: {
              source: dataSourceName,
              table,
              configuration: Object.assign(
                metadataTable?.configuration || {},
                config,
              ),
            },
          },
        },
        {
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: 'Configuration saved!',
            });
          },
          onError: (err) => {
            showErrorNotification({
              title: 'Failed to save configuration.',
              error: err,
            });
          },
        },
      );
    },
    [
      dataSourceName,
      mutateAsync,
      metadataTable,
      resource_version,
      source,
      table,
    ],
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
    [updateTableConfiguration],
  );

  return {
    updateTableConfiguration,
    updateCustomRootFields,
    source,
    resource_version,
    ...rest,
  };
};
