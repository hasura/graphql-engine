import * as React from 'react';
import { useQuery } from 'react-query';
import { schemaRegsitryControlPlaneClient } from '../utils';
import { FETCH_REGISTRY_SCHEMA_QUERY } from '../queries';
import { GetRegistrySchemaResponseWithError } from '../types';
import {
  FETCH_REGISTRY_SCHEMA_QUERY_NAME,
  SCHEMA_REGISTRY_REFRESH_TIME,
} from '../constants';

type FetchSchemaResponse =
  | {
      kind: 'loading';
    }
  | {
      kind: 'error';
      message: string;
    }
  | {
      kind: 'success';
      response: NonNullable<GetRegistrySchemaResponseWithError['data']>;
    };

export const useGetSchema = (schemaId: string): FetchSchemaResponse => {
  const fetchRegistrySchemaQueryFn = (schemaId: string) => {
    return schemaRegsitryControlPlaneClient.query<
      GetRegistrySchemaResponseWithError,
      { schemaId: string }
    >(FETCH_REGISTRY_SCHEMA_QUERY, {
      schemaId: schemaId,
    });
  };

  const { data, error, isLoading, refetch } = useQuery({
    queryKey: FETCH_REGISTRY_SCHEMA_QUERY_NAME,
    queryFn: () => fetchRegistrySchemaQueryFn(schemaId),
    refetchOnMount: 'always',
    refetchOnWindowFocus: true,
    staleTime: SCHEMA_REGISTRY_REFRESH_TIME,
  });
  React.useEffect(() => {
    refetch();
  }, [schemaId]);
  if (isLoading) {
    return {
      kind: 'loading',
    };
  }

  if (error || !data || !!data?.errors || !data?.data) {
    return {
      kind: 'error',
      message: 'error',
    };
  }
  return {
    kind: 'success',
    response: data.data,
  };
};
