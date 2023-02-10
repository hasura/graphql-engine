import { generateQueryKeys } from '@/features/DatabaseRelationships/utils/queryClientUtils';
import { useMetadataMigration } from '@/features/MetadataAPI';
import { useCallback, useMemo } from 'react';
import { useQueryClient } from 'react-query';
import { DatabaseConnection } from '../types';

export const useManageDatabaseConnection = ({
  onSuccess,
  onError,
}: {
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) => {
  const queryClient = useQueryClient();
  const { mutate, ...rest } = useMetadataMigration();
  const mutationOptions = useMemo(
    () => ({
      onSuccess: () => {
        queryClient.invalidateQueries(generateQueryKeys.metadata());
        onSuccess?.();
      },
      onError: (err: Error) => {
        onError?.(err);
      },
    }),
    [onError, onSuccess, queryClient]
  );

  const createConnection = useCallback(
    async (databaseConnection: DatabaseConnection) => {
      mutate(
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
    },
    [mutate, mutationOptions]
  );

  const editConnection = useCallback(
    async (databaseConnection: DatabaseConnection) => {
      mutate(
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
    [mutate, mutationOptions]
  );

  return { createConnection, editConnection, ...rest };
};
