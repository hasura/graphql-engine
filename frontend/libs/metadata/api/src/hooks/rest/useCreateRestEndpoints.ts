import { useCallback } from 'react';
import {
  type EndpointType,
  useRestEndpointDefinitions,
} from './useRestEndpointDefinitions';
import { Table } from '@hasura/shared/types';
import { useMetadata, useMetadataMigration } from '../metadata';
import { createAddOperationToQueryCollectionMetadataArgs } from '../queryCollections';
import { EndpointDefinition } from './useRestEndpointDefinitions/types';

export const useCreateRestEndpoints = (props: {
  dataSourceName: string;
  table: Table;
}) => {
  const { data: endpointDefinitions } = useRestEndpointDefinitions(props);
  const { mutate, ...rest } = useMetadataMigration();
  const { data: metadata } = useMetadata();

  const createRestEndpoints = useCallback(
    (
      table: string,
      types: EndpointType[],
      options?: Parameters<typeof mutate>[1],
    ) => {
      const endpoints = types
        .map((type) => endpointDefinitions?.[table]?.[type])
        .filter((a) => a) as EndpointDefinition[];

      return mutate(
        {
          query: {
            type: 'bulk',
            resource_version: metadata?.resource_version,
            args: [
              ...createAddOperationToQueryCollectionMetadataArgs(
                'allowed-queries',
                endpoints?.map((endpoint) => endpoint.query),
                metadata,
              ),
              ...endpoints.map((endpoint) => ({
                type: 'create_rest_endpoint' as const,
                args: endpoint.restEndpoint,
              })),
            ],
          },
        },
        {
          ...options,
        },
      );
    },
    [endpointDefinitions, mutate, metadata],
  );

  return { createRestEndpoints, endpointDefinitions, ...rest };
};
