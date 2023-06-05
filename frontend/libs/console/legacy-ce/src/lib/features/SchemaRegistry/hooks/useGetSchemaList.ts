import * as React from 'react';
import { useQuery } from 'react-query';
import { FETCH_REGSITRY_SCHEMAS_QUERY } from '../queries';
import { schemaRegsitryControlPlaneClient } from '../utils';
import { GetSchemaListResponseWithError } from '../types';
import {
  FETCH_REGISTRY_SCHEMAS_QUERY_NAME,
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
      response: NonNullable<GetSchemaListResponseWithError['data']>;
    };

export const useGetSchemaList = (projectId: string): FetchSchemaResponse => {
  const fetchRegistrySchemasQueryFn = (projectId: string) => {
    return schemaRegsitryControlPlaneClient.query<
      GetSchemaListResponseWithError,
      { projectId: string }
    >(FETCH_REGSITRY_SCHEMAS_QUERY, {
      projectId: projectId,
    });
  };

  const { data, error, isLoading } = useQuery({
    queryKey: FETCH_REGISTRY_SCHEMAS_QUERY_NAME,
    queryFn: () => fetchRegistrySchemasQueryFn(projectId),
    refetchOnMount: 'always',
    refetchOnWindowFocus: true,
    staleTime: SCHEMA_REGISTRY_REFRESH_TIME,
  });

  if (isLoading) {
    return {
      kind: 'loading',
    };
  }

  if (error || !data || !!data.errors || !data.data) {
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
