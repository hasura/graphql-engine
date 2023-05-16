import { useCallback, useMemo } from 'react';
import { useQueryClient } from 'react-query';
import { exportMetadata } from '../../../metadata/actions';
import { useAppDispatch } from '../../../storeHooks';
import { generateQueryKeys } from '../../DatabaseRelationships/utils/queryClientUtils';
import { useMetadataMigration } from '../../MetadataAPI';
import { DatabaseConnection } from '../types';
import { usePushRoute } from './usePushRoute';
import {
  sendConnectDatabaseTelemetryEvent,
  transformErrorResponse,
} from '../utils';
import { useHttpClient } from '../../Network';
import { Driver } from '../../../dataSources';

export const useManageDatabaseConnection = ({
  onSuccess,
  onError,
}: {
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) => {
  const queryClient = useQueryClient();
  const { mutateAsync, ...rest } = useMetadataMigration({
    errorTransform: transformErrorResponse,
  });
  const push = usePushRoute();
  const dispatch = useAppDispatch();
  const httpClient = useHttpClient();

  const mutationOptions = useMemo(
    () => ({
      onSuccess: () => {
        queryClient.invalidateQueries(generateQueryKeys.metadata());
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
    [dispatch, onError, onSuccess, push, queryClient]
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
    [mutateAsync, mutationOptions]
  );

  const editConnection = useCallback(
    async (databaseConnection: DatabaseConnection) => {
      mutateAsync(
        {
          query: {
            type: `${databaseConnection.driver}_add_source`,
            args: {
              name: databaseConnection.details.name,
              configuration: databaseConnection.details.configuration,
              customization: databaseConnection.details.customization,
              replace_configuration: true,
            },
          },
        },
        mutationOptions
      );
    },
    [mutateAsync, mutationOptions]
  );

  return { createConnection, editConnection, ...rest };
};
