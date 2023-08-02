import * as React from 'react';
import { useQuery } from 'react-query';
import { FETCH_REGISTRY_SCHEMAS_QUERY } from '../queries';
import { schemaRegsitryControlPlaneClient } from '../utils';
import { GetSchemaListResponseWithError } from '../types';
import {
  FETCH_REGISTRY_SCHEMAS_QUERY_NAME,
  SCHEMA_REGISTRY_REFRESH_TIME,
  SCHEMA_LIST_FETCH_BATCH_SIZE,
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
      response: NonNullable<
        GetSchemaListResponseWithError['data']
      >['schema_registry_dumps'];
      loadMore: VoidFunction;
      isLoadingMore: boolean;
      shouldLoadMore: boolean;
    };

export const useGetSchemaList = (projectId: string): FetchSchemaResponse => {
  const [dumps, setDumps] = React.useState<
    NonNullable<GetSchemaListResponseWithError['data']>['schema_registry_dumps']
  >([]);
  const [loadingMore, setLoadingMore] = React.useState(false);
  const [shouldLoadMore, setShouldLoadMore] = React.useState(true);

  let cursor = 'now()';
  if (dumps && dumps.length > 0) {
    cursor = dumps[dumps.length - 1].change_recorded_at;
  }

  const fetchRegistrySchemasQueryFn = React.useCallback(
    (projectId: string) => {
      return schemaRegsitryControlPlaneClient.query<
        GetSchemaListResponseWithError,
        { projectId: string; cursor: string; limit: number }
      >(FETCH_REGISTRY_SCHEMAS_QUERY, {
        projectId: projectId,
        cursor: cursor,
        limit: SCHEMA_LIST_FETCH_BATCH_SIZE,
      });
    },
    [cursor]
  );

  const { data, error, isLoading, refetch } = useQuery({
    queryKey: FETCH_REGISTRY_SCHEMAS_QUERY_NAME,
    queryFn: () => fetchRegistrySchemasQueryFn(projectId),
    refetchOnMount: 'always',
    refetchOnWindowFocus: true,
    staleTime: SCHEMA_REGISTRY_REFRESH_TIME,
    onSettled: () => {
      setLoadingMore(false);
    },
    onSuccess: response => {
      if (response && response.data && response.data.schema_registry_dumps) {
        const dumps = response.data.schema_registry_dumps;
        setDumps(d => [...(d || []), ...dumps]);
        if (dumps.length < SCHEMA_LIST_FETCH_BATCH_SIZE) {
          setShouldLoadMore(false);
        }
      }
    },
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
    response: dumps,
    loadMore: () => {
      setLoadingMore(true);
      refetch();
    },
    isLoadingMore: loadingMore,
    shouldLoadMore,
  };
};
