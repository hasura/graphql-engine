import { useMetadataMigration } from '../../MetadataAPI';
import { useCallback, useMemo } from 'react';
import { useQueryClient } from 'react-query';
import { useMetadata } from '../../hasura-metadata-api';
import { LocalRelationship } from '../types';
import {
  generateCreateLocalRelationshipWithManualConfigurationRequest,
  generateDeleteLocalRelationshipRequest,
  generateRenameLocalRelationshipRequest,
} from '../utils/generateRequest';
import { generateQueryKeys } from '../utils/queryClientUtils';
import { Table } from '../../hasura-metadata-types';

export const useManageLocalRelationship = ({
  dataSourceName,
  table,
  onSuccess,
  onError,
}: {
  dataSourceName: string;
  table: Table;
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
        queryClient.invalidateQueries(
          generateQueryKeys.suggestedRelationships({ dataSourceName, table })
        );
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
