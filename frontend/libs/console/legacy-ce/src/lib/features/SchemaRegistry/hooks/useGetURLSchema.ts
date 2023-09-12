import * as React from 'react';
import { useQuery } from 'react-query';
import { schemaRegsitryControlPlaneClient } from '../utils';
import { FETCH_URL_REGISTRY_SCHEMA_QUERY } from '../queries';
import {
  GetURLRegistrySchemaResponseWithError,
  SchemaChangeListDumpWithSiblingSchema,
} from '../types';
import { FETCH_URL_REGISTRY_SCHEMA_QUERY_NAME } from '../constants';

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
      response: SchemaChangeListDumpWithSiblingSchema[];
    };

export const useGetURLSchema = (schemaId: string): FetchSchemaResponse => {
  const fetchRegistrySchemaQueryFn = (schemaId: string) => {
    return schemaRegsitryControlPlaneClient.query<
      GetURLRegistrySchemaResponseWithError,
      { schemaId: string }
    >(FETCH_URL_REGISTRY_SCHEMA_QUERY, {
      schemaId: schemaId,
    });
  };

  const { data, error, isLoading } = useQuery({
    queryKey: FETCH_URL_REGISTRY_SCHEMA_QUERY_NAME,
    queryFn: () => fetchRegistrySchemaQueryFn(schemaId),
    refetchOnMount: 'always',
    refetchOnWindowFocus: true,
  });
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
  if (data.data?.schema_registry_dumps_v2.length) {
    return {
      kind: 'success',
      response: data.data.schema_registry_dumps_v2,
    };
  }
  return {
    kind: 'success',
    response: data.data.schema_registry_dumps,
  };
};
