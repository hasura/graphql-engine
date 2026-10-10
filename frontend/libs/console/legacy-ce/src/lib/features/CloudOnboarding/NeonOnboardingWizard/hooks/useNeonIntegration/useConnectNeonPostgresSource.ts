import { useCallback } from 'react';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { useMetadataMigration } from '@hasura/metadata/api';
import { sendInitialDBStateTelemetry } from './telemetry';
import { hasuraToast } from '@hasura/shared/ui';
import { useAppContext } from '@hasura/shared/context';

export const useConnectNeonPostgresSource = () => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();
  const mutation = useMetadataMigration();

  return useCallback(
    (
      {
        envVar,
        shouldShowNotification = true,
      }: {
        envVar: string;
        shouldShowNotification: boolean;
      },
      onSuccess: () => void,
      onError: () => void,
    ) => {
      const databaseURL = { from_env: envVar.trim() };
      return mutation.mutate(
        {
          query: {
            type: 'pg_update_source',
            args: {
              configuration: {
                connection_info: {
                  database_url: databaseURL,
                },
              },
            },
          },
        },
        {
          onSuccess: (data) => {
            // send DB initial state to telemetry
            sendInitialDBStateTelemetry(endpoints, fetchJson, {
              name: 'default',
              kind: 'postgres',
            });

            if (shouldShowNotification) {
              hasuraToast({
                type: 'success',
                title: `Data source added successfully!`,
              });
            }

            onSuccess?.();
          },
          onError: (err) => {
            onError?.();
          },
        },
      );
    },
    [mutation],
  );
};
