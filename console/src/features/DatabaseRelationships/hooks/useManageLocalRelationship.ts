import { useMetadataMigration } from '@/features/MetadataAPI';
import { useCallback, useMemo } from 'react';
import { useQueryClient } from 'react-query';
import {
  useMetadataSource,
  useResourceVersion,
} from '@/features/hasura-metadata-api';
import { LocalRelationship } from '../types';
import {
  generateCreateLocalRelationshipWithManualConfigurationRequest,
  generateDeleteLocalRelationshipRequest,
  generateRenameLocalRelationshipRequest,
} from '../utils/generateRequest';
import { generateQueryKeys } from '../utils/queryClientUtils';

export const useManageLocalRelationship = ({
  dataSourceName,
  onSuccess,
  onError,
}: {
  dataSourceName: string;
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) => {
  const { data: resource_version } = useResourceVersion();
  const { data: metadataSource } = useMetadataSource(dataSourceName);
  const queryClient = useQueryClient();
  const { mutate, ...rest } = useMetadataMigration();
  const mutationOptions = useMemo(
    () => ({
      onSuccess: () => {
        queryClient.refetchQueries(generateQueryKeys.metadata());
        onSuccess?.();
      },
      onError: (err: Error) => {
        onError?.(err);
      },
    }),
    [onError, onSuccess, queryClient]
  );

  const driver = metadataSource?.kind;

  const renameRelationship = useCallback(
    async (relationship: LocalRelationship, newName: string) => {
      if (!resource_version || !driver) throw Error('Metadata not ready');

      mutate(
        {
          query: generateRenameLocalRelationshipRequest({
            resource_version,
            relationship,
            driver,
            newName,
          }),
        },
        mutationOptions
      );
    },
    [driver, mutate, mutationOptions, resource_version]
  );

  const createRelationship = useCallback(
    async (relationship: LocalRelationship) => {
      if (!resource_version || !driver) throw Error('Metadata not ready');

      mutate(
        {
          query: generateCreateLocalRelationshipWithManualConfigurationRequest({
            resource_version,
            relationship,
            driver,
          }),
        },
        mutationOptions
      );
    },
    [driver, mutate, mutationOptions, resource_version]
  );

  const deleteRelationship = useCallback(
    async (relationship: LocalRelationship) => {
      if (!resource_version || !driver) throw Error('Metadata not ready');

      mutate(
        {
          query: generateDeleteLocalRelationshipRequest({
            driver,
            resource_version,
            relationship,
          }),
        },
        mutationOptions
      );
    },
    [driver, mutate, mutationOptions, resource_version]
  );

  return {
    renameRelationship,
    deleteRelationship,
    createRelationship,
    ...rest,
  };
};
