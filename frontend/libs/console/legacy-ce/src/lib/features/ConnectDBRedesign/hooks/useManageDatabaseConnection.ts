import { useCallback, useMemo } from 'react';
import { Driver } from '../../../dataSources';
import { exportMetadata } from '../../../metadata/actions';
import { useAppDispatch } from '../../../storeHooks';
import { useMetadataMigration } from '../../MetadataAPI';
import { useHttpClient } from '../../Network';
import { useMetadata } from '../../hasura-metadata-api';
import { DatabaseConnection } from '../types';
import {
  sendConnectDatabaseTelemetryEvent,
  transformErrorResponse,
} from '../utils';
import { usePushRoute } from './usePushRoute';

export const useManageDatabaseConnection = ({
  onSuccess,
  onError,
}: {
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) => {
  const { mutateAsync, ...rest } = useMetadataMigration({
    errorTransform: transformErrorResponse,
  });
  const { data: resource_version } = useMetadata(m => m.resource_version);
  const push = usePushRoute();
  const dispatch = useAppDispatch();
  const httpClient = useHttpClient();

  const mutationOptions = useMemo(
    () => ({
      onSuccess: () => {
        onSuccess?.();

        // this code is only for the demo
        push('/data/manage');
        dispatch(exportMetadata());
      },
      onError: (err: Error) => {
        console.log('~', err);
        onError?.(err);
      },
    }),
    [dispatch, onError, onSuccess, push]
  );

  const createConnection = useCallback(
    async (databaseConnection: DatabaseConnection) => {
      await mutateAsync(
        {
          query: {
            type: `${databaseConnection.driver}_add_source`,
            args: {
              name: databaseConnection.details.name,
              configuration: databaseConnection.details.configuration,
              customization: databaseConnection.details.customization,
            },
          },
        },
        mutationOptions
      );
      sendConnectDatabaseTelemetryEvent({
        httpClient,
        driver: databaseConnection.driver as Driver,
        dataSourceName: databaseConnection.details.name,
      });
    },
    [httpClient, mutateAsync, mutationOptions]
  );

  const editConnection = useCallback(
    async (
      databaseConnection: DatabaseConnection & { originalName: string }
    ) => {
      const renameConnectionPayload = {
        type: 'rename_source',
        args: {
          name: databaseConnection.originalName,
          new_name: databaseConnection.details.name,
        },
      };

      const updateConfigurationPayload = {
        type: `${databaseConnection.driver}_add_source`,
        args: {
          name: databaseConnection.details.name,
          configuration: databaseConnection.details.configuration,
          customization: databaseConnection.details.customization,
          replace_configuration: true,
        },
      };

      mutateAsync(
        {
          query: {
            type: 'bulk',
            source: databaseConnection.originalName,
            resource_version,
            args:
              databaseConnection.details.name ===
              databaseConnection.originalName
                ? [updateConfigurationPayload]
                : [renameConnectionPayload, updateConfigurationPayload],
          },
        },
        mutationOptions
      );
    },
    [mutateAsync, mutationOptions, resource_version]
  );

  return { createConnection, editConnection, ...rest };
};
