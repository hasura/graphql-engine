import { useCallback } from 'react';
import { useMetadata, useMetadataMigration } from '../../MetadataAPI';
import { createAddOperationToQueryCollectionMetadataArgs } from '../../QueryCollections/hooks';
import {
  type EndpointType,
  useRestEndpointDefinitions,
  EndpointDefinition,
} from './useRestEndpointDefinitions';

export const useCreateRestEndpoints = () => {
  const { data: endpointDefinitions } = useRestEndpointDefinitions();

  const { mutate, ...rest } = useMetadataMigration();

  const { data: metadata, isSuccess: isReady } = useMetadata();

  const createRestEndpoints = useCallback(
    (
      table: string,
      types: EndpointType[],
      options?: Parameters<typeof mutate>[1]
    ) => {
      const endpoints = types
        .map(type => endpointDefinitions?.[table]?.[type])
        .filter(a => a) as EndpointDefinition[];

      return mutate(
        {
          query: {
            type: 'bulk',
            ...(metadata?.resource_version && {
              resource_version: metadata.resource_version,
            }),
            args: [
              ...createAddOperationToQueryCollectionMetadataArgs(
                'allowed-queries',
                endpoints?.map(endpoint => endpoint.query),
                metadata
              ),
              ...endpoints.map(endpoint => ({
                type: 'create_rest_endpoint',
                args: endpoint.restEndpoint,
              })),
            ],
          },
        },
        {
          ...options,
        }
      );
    },
    [endpointDefinitions, mutate, metadata]
  );

  return { createRestEndpoints, endpointDefinitions, isReady, ...rest };
};
