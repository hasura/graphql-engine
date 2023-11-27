import { useCallback } from 'react';
import { useMetadata, useMetadataMigration } from '../../MetadataAPI';
import { createAddOperationToQueryCollectionMetadataArgs } from '../../QueryCollections/hooks';
import {
  type EndpointType,
  useRestEndpointDefinitions,
  EndpointDefinition,
} from './useRestEndpointDefinitions';
import { Table } from '../../hasura-metadata-types';

export const useCreateRestEndpoints = (props: {
  dataSourceName: string;
  table: Table;
}) => {
  const { data: endpointDefinitions } = useRestEndpointDefinitions(props);

  const { mutate, ...rest } = useMetadataMigration();

  const { data: metadata, isSuccess: isReady } = useMetadata();

  const createRestEndpoints = useCallback(
    (
      dataSourceName: string,
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
