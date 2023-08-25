import * as React from 'react';
import { useQuery } from 'react-query';
import { FETCH_SCHEMA_CHANGE_LIST_QUERY } from '../queries';
import { schemaRegsitryControlPlaneClient } from '../utils';
import {
  GetSchemaChangeListResponseWithError,
  GetSchemaListResponseWithError,
  SchemaRegistryDumpsAggregate,
} from '../types';
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
      response: NonNullable<
        GetSchemaListResponseWithError['data']
      >['schema_registry_dumps'];
      totalCount: number;
    };

export const useGetSchemaChangeList = (
  projectId: string,
  limit: number,
  offset: number,
  schemaId: string
): FetchSchemaResponse => {
  const [dumps, setDumps] = React.useState<
    NonNullable<GetSchemaListResponseWithError['data']>['schema_registry_dumps']
  >([]);
  const [totalCount, setTotalCount] =
    React.useState<
      NonNullable<SchemaRegistryDumpsAggregate['aggregate']>['count']
    >(0);

  const fetchRegistrySchemasQueryFn = React.useCallback(
    (projectId: string, limit: number, offset: number, schemaId: string) => {
      return schemaRegsitryControlPlaneClient.query<
        GetSchemaChangeListResponseWithError,
        { projectId: string; limit: number; offset: number; schemaId: string }
      >(FETCH_SCHEMA_CHANGE_LIST_QUERY, {
        projectId: projectId,
        limit: limit,
        offset: offset,
        schemaId: schemaId,
      });
    },
    [limit, offset]
  );

  const { data, error, isLoading, refetch } = useQuery({
    queryKey: FETCH_REGISTRY_SCHEMAS_QUERY_NAME,
    queryFn: () =>
      fetchRegistrySchemasQueryFn(projectId, limit, offset, schemaId),
    refetchOnMount: 'always',
    refetchOnWindowFocus: true,
    staleTime: SCHEMA_REGISTRY_REFRESH_TIME,
    onSuccess: response => {
      setDumps([]);
      setTotalCount(0);
      if (response && response.data && response.data.schema_change_list) {
        const tempDumps = response.data.schema_change_list;
        setDumps(tempDumps);
        if (response.data.current_schema_card[0]) {
          //adding to the top of the array so that this opens
          tempDumps.unshift(response.data.current_schema_card[0]);
          setDumps(tempDumps);
        }
      }
      if (
        response &&
        response.data &&
        response.data.schema_registry_dumps_aggregate?.aggregate?.count
      ) {
        const count =
          response.data.schema_registry_dumps_aggregate?.aggregate?.count;
        setTotalCount(count);
      }
    },
  });

  React.useEffect(() => {
    refetch();
  }, [offset, limit]);

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
    totalCount: totalCount,
  };
};
