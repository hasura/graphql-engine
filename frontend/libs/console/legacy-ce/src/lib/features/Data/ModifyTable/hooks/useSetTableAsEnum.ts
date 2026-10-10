import { useCallback } from 'react';
import {
  useErrorNotification,
  useMetadata,
  useMetadataMigration,
} from '@hasura/metadata/api';
import { Table } from '@hasura/shared/types';
import { hasuraToast } from '@hasura/shared/ui';
import { getDriverPrefix, MetadataSelectors } from '@hasura/metadata/helpers';

/**
 * Toggles a table's `is_enum` flag via the backend-prefixed
 * `<driver>_set_table_is_enum` metadata API (e.g. `pg_set_table_is_enum`).
 *
 * Consumes the shared `useMetadataMigration` primitive unchanged.
 */
export const useSetTableAsEnum = (dataSourceName: string, table: Table) => {
  const { mutate, ...rest } = useMetadataMigration();
  const showErrorNotification = useErrorNotification();

  const { data } = useMetadata((m) => ({
    source: MetadataSelectors.findMetadataSource(dataSourceName, m),
    resource_version: m.resource_version,
  }));

  const { source, resource_version } = data || {};

  const setTableAsEnum = useCallback(
    (isEnum: boolean, options?: { onSuccess?: () => void }) => {
      if (!source?.kind) {
        throw Error('Data Source not found!');
      }

      return mutate(
        {
          query: {
            resource_version,
            type: `${getDriverPrefix(source.kind)}_set_table_is_enum`,
            args: {
              source: dataSourceName,
              table,
              is_enum: isEnum,
            },
          },
        },
        {
          onSuccess: () => {
            hasuraToast({
              type: 'success',
              title: 'Success!',
              message: isEnum
                ? 'Table set as an enum'
                : 'Table is no longer an enum',
            });
            options?.onSuccess?.();
          },
          onError: (err) => {
            showErrorNotification({
              title: 'Failed to update enum setting',
              error: err,
            });
          },
        },
      );
    },
    [
      dataSourceName,
      mutate,
      resource_version,
      showErrorNotification,
      source,
      table,
    ],
  );

  return { setTableAsEnum, ...rest };
};
