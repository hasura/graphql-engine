import { useMetadataMigration } from '../../MetadataAPI';
import { useCallback, useMemo } from 'react';
import { useQueryClient } from 'react-query';
import { useMetadata } from '../../hasura-metadata-api';
import { RemoteSchemaRelationship } from '../types';
import {
  generateRemoteRelationshipCreateRequest,
  generateRemoteRelationshipEditRequest,
  generateRemoteRelationshipDeleteRequest,
} from '../utils/generateRequest';
import { generateQueryKeys } from '../utils/queryClientUtils';

export const useManageRemoteSchemaRelationship = ({
  dataSourceName,
  onSuccess,
  onError,
}: {
  dataSourceName: string;
  onSuccess?: () => void;
  onError?: (err: Error) => void;
}) => {
  const { data } = useMetadata(m => {
    return {
      resource_version: m.resource_version,
      source: m.metadata.sources.find(s => s.name === dataSourceName),
    };
  });

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

  const metadataSource = data?.source;
  const resource_version = data?.resource_version;
  const driver = metadataSource?.kind;

  const createRelationship = useCallback(
    async (relationship: RemoteSchemaRelationship) => {
      if (!resource_version || !driver) throw Error('Metadata not ready');

      mutate(
        {
          query: generateRemoteRelationshipCreateRequest({
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

  const editRelationship = useCallback(
    async (relationship: RemoteSchemaRelationship) => {
      if (!resource_version || !driver) throw Error('Metadata not ready');

      mutate(
        {
          query: generateRemoteRelationshipEditRequest({
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
    async (relationship: RemoteSchemaRelationship) => {
      if (!resource_version || !driver) throw Error('Metadata not ready');
      mutate(
        {
          query: generateRemoteRelationshipDeleteRequest({
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
    editRelationship,
    deleteRelationship,
    createRelationship,
    ...rest,
  };
};
