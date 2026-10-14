import React from 'react';
import { useQuery } from '@tanstack/react-query';
import { schemaRegsitryControlPlaneClient } from '../utils';

import { FETCH_SCHEMA_REGSITRY_DUMPS_V2_INFO_QUERY } from '../queries';
import { GetSchemaRegstiryDumpsV2AggregateResponseWithError } from '../types';
import {
  FETCH_SCHEMA_REGSITRY_DUMPS_V2_INFO_QUERY_NAME,
  SCHEMA_REGISTRY_REFRESH_TIME,
} from '../constants';

type GetSchemaRegistryListResponse =
  | {
      kind: 'loading';
    }
  | {
      kind: 'error';
      message: string;
    }
  | {
      kind: 'success';
      v2Count: number;
      v2Cursor: string;
    };

export const useGetV2Info = (
  projectId: string,
): GetSchemaRegistryListResponse => {
  /**
   * Fetch the aggregate count for Schema Registry Dumps V2 along with the change_recorded_at of the last entry in the V2 table
   */
  const fetchSchemaRegistryDumpsV2AggregateFn = React.useCallback(
    (projectId: string) => {
      return schemaRegsitryControlPlaneClient.query<
        GetSchemaRegstiryDumpsV2AggregateResponseWithError,
        { projectId: string }
      >(FETCH_SCHEMA_REGSITRY_DUMPS_V2_INFO_QUERY, {
        projectId: projectId,
      });
    },
    [],
  );

  const {
    data: v2InfoData,
    error: v2InfoError,
    isLoading: v2InfoLoading,
  } = useQuery({
    queryKey: FETCH_SCHEMA_REGSITRY_DUMPS_V2_INFO_QUERY_NAME,
    queryFn: () => fetchSchemaRegistryDumpsV2AggregateFn(projectId),
    refetchOnMount: 'always',
    refetchOnWindowFocus: false,
    staleTime: SCHEMA_REGISTRY_REFRESH_TIME,
  });

  // Both values are fully derived from the fetched aggregate data, so compute
  // them during render instead of mirroring into state from an effect.
  const v2Count =
    v2InfoData?.data?.schema_registry_dumps_v2_aggregate?.aggregate?.count ?? 0;

  const lastV2EntryCursor =
    (v2InfoData?.data?.schema_registry_dumps_v2?.length &&
      v2InfoData.data.schema_registry_dumps_v2[0].change_recorded_at) ||
    'now()';

  if (v2InfoLoading) {
    return {
      kind: 'loading',
    };
  }

  if (v2InfoError || !!v2InfoData?.errors) {
    return {
      kind: 'error',
      message: 'error',
    };
  }

  return {
    kind: 'success',
    v2Count: v2Count,
    v2Cursor: lastV2EntryCursor,
  };
};
