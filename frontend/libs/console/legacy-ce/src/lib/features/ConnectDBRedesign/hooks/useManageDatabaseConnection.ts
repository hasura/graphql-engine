import { useCallback } from 'react';
import { useMetadataMigration, useMetadata } from '@hasura/metadata/api';
import type { DatabaseConnection } from '../types';
import { sendConnectDatabaseTelemetryEvent } from '../utils';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { SupportedDriver } from '@hasura/shared/types';
import { useAppContext } from '@hasura/shared/context';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { hasuraToast } from '@hasura/shared/ui';
import { getErrorMessage } from '@hasura/shared/utils';

export const useManageDatabaseConnection = ({
  onSuccess,
  onError,
}: {
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) => {
  const { endpoints } = useAppContext();
  const { mutate, ...rest } = useMetadataMigration({});
  const { data: resource_version, refetch: refetchMetadata } = useMetadata(
    (m) => m.resource_version,
  );
  const fetchJson = useAuthFetchJson();

  const createConnection = useCallback(
    async (databaseConnection: DatabaseConnection) => {
      try {
        await mutate(
          {
            query: {
              type: `${getDriverPrefix(databaseConnection.driver)}_add_source` as const,
              args: {
                name: databaseConnection.details.name,
                configuration: databaseConnection.details.configuration,
                customization: databaseConnection.details.customization,
              },
            },
          },
          {
            onSuccess: () => {
              onSuccess?.();
              refetchMetadata();
            },
            onError: (err: Error) => {
              hasuraToast({
                type: 'error',
                title: 'Failed to connect database',
                message: getErrorMessage(err),
              });
              onError?.(err);
            },
          },
        );
      } catch {
        //console.log('Error in create connection mutation: ', mutationError);
        // if there's an error with the connection mutation, return and don't send telemetry request
        return;
      }

      await sendConnectDatabaseTelemetryEvent({
        fetchJson,
        endpoints,
        driver: databaseConnection.driver as SupportedDriver,
        dataSourceName: databaseConnection.details.name,
      }).catch(() => {
        //console.log('Error in create connection telemetry: ', telemetryError);
      });
    },
    [fetchJson, mutate, onError],
  );

  const editConnection = useCallback(
    async (
      databaseConnection: DatabaseConnection & { originalName: string },
    ) => {
      const renameConnectionPayload = {
        type: 'rename_source' as const,
        args: {
          name: databaseConnection.originalName,
          new_name: databaseConnection.details.name,
        },
      };

      const updateConfigurationPayload = {
        type: `${getDriverPrefix(databaseConnection.driver)}_add_source` as const,
        args: {
          name: databaseConnection.details.name,
          configuration: databaseConnection.details.configuration,
          customization: databaseConnection.details.customization,
          replace_configuration: true,
        },
      };

      await mutate(
        {
          query: {
            type: 'bulk',
            resource_version,
            args:
              databaseConnection.details.name ===
              databaseConnection.originalName
                ? [updateConfigurationPayload]
                : [renameConnectionPayload, updateConfigurationPayload],
          },
        },
        {
          onSuccess: () => {
            onSuccess?.();
            refetchMetadata();
          },
          onError: (err: Error) => {
            hasuraToast({
              type: 'error',
              title: 'Failed to edit database connection',
              message: getErrorMessage(err),
            });
            onError?.(err);
          },
        },
      );
    },
    [mutate, resource_version],
  );

  return { createConnection, editConnection, ...rest };
};
