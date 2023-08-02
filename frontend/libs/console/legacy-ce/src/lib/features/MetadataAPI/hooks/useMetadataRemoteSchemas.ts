import React from 'react';
import { useQueryClient } from 'react-query';
import { buildClientSchema, GraphQLSchema } from 'graphql';
import Endpoints from '../../../Endpoints';
import { Api } from '../../../hooks/apiUtils';
import { APIError } from '../../../hooks/error';
import { useSelector } from 'react-redux';

import { MetadataSelector } from './metadataSelectors';
import { useMetadata } from './useMetadata';

import type { SchemaResponse } from '../types';

export const useGetAllRemoteSchemaRelationships = () => {
  return useMetadata(MetadataSelector.getAllRemoteSchemaRelationships());
};

export const useListRemoteSchemas = () => {
  return useMetadata(MetadataSelector.listRemoteSchemas());
};

export const useRemoteSchema = () => {
  const client = useQueryClient();
  // Needed to avoid circular dependency
  const headers = useSelector<any>(state => state.tables.dataHeaders) as Record<
    string,
    string
  >;

  const [data, setData] = React.useState<GraphQLSchema>();
  const [isLoading, setIsLoading] = React.useState(false);
  const [isError, setIsError] = React.useState(false);
  const [error, setError] = React.useState<APIError>();

  const fetchSchema = React.useCallback(
    async (name: string) => {
      setIsLoading(true);
      const body = {
        type: 'introspect_remote_schema',
        args: {
          name,
        },
      };

      const queryFn = () => {
        return Api.post<SchemaResponse>({
          headers,
          body,
          url: Endpoints.metadata,
        });
      };

      try {
        const result = await client.fetchQuery({
          queryKey: `introspect_remote_schema_${name}`,
          queryFn,
        });

        const schema = buildClientSchema(result.data);
        setData(schema);
        setIsLoading(false);

        return result;
      } catch (e) {
        setIsLoading(false);
        setError(e as APIError);
        setIsError(true);
        return undefined;
      }
    },
    [client, headers]
  );

  return { fetchSchema, data, isLoading, isError, error };
};

export const useGetRemoteSchemaRelationship = (sourceRemoteSchema: string) => {
  return useMetadata(
    MetadataSelector.getRemoteSchemaRelationship(sourceRemoteSchema)
  );
};
