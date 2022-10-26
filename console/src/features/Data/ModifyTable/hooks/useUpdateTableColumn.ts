import { exportMetadata, TableColumn } from '@/features/DataSource';
import { Table, useMetadataMigration } from '@/features/MetadataAPI';
import { useHttpClient } from '@/features/Network';
import { areTablesEqual } from '@/features/RelationshipsTable';
import { useFireNotification } from '@/new-components/Notifications';
import { useCallback } from 'react';
import { useQueryClient } from 'react-query';
import { Schema } from '../components/EditTableColumnDialog';

export const useUpdateTableColumn = ({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) => {
  const httpClient = useHttpClient();

  const { mutate, ...rest } = useMetadataMigration();

  const { fireNotification } = useFireNotification();

  const queryClient = useQueryClient();

  const updateTableColumn = useCallback(
    async ({
      column,
      updatedConfig,
      customOnSuccess,
      customOnError,
    }: {
      column: TableColumn;
      updatedConfig: Schema;
      customOnSuccess?: () => void;
      customOnError?: (err: unknown) => void;
    }) => {
      const { metadata, resource_version } = await exportMetadata({
        httpClient,
      });

      if (!metadata) throw Error('Cannot find metadata info');

      const metadataSource = metadata.sources.find(
        s => s.name === dataSourceName
      );

      if (!metadataSource) throw Error('Cannot find source in metadata');

      const metadataTable = metadataSource.tables.find(t =>
        areTablesEqual(t.table, table)
      );

      if (!metadataTable) throw Error('Cannot find table');

      const driver = metadataSource.kind;
      const type = 'set_table_customization';

      mutate(
        {
          query: {
            resource_version,
            type: `${driver}_${type}`,
            args: {
              table,
              source: dataSourceName,
              configuration: {
                ...metadataTable.configuration,
                column_config: {
                  ...metadataTable.configuration?.column_config,
                  [column.name]: updatedConfig,
                },
              },
            },
          },
        },
        {
          onSuccess: () => {
            fireNotification({
              title: 'Success!',
              message: 'Successfully updated column!',
              type: 'success',
            });

            queryClient.refetchQueries([
              'modify',
              'columns',
              dataSourceName,
              table,
            ]);

            if (customOnSuccess) {
              customOnSuccess();
            }
          },
          onError: err => {
            fireNotification({
              title: 'Error!',
              message: JSON.stringify(err),
              type: 'error',
            });
            if (customOnError) {
              customOnError(err);
            }
          },
        }
      );
    },
    [httpClient, mutate, table, dataSourceName, fireNotification, queryClient]
  );

  return { updateTableColumn, ...rest };
};
