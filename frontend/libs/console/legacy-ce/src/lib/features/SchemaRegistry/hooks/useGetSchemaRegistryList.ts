import React, { useState } from 'react';
import { useQuery } from 'react-query';
import { schemaRegsitryControlPlaneClient } from '../utils';

import {
  FETCH_SCHEMA_REGISTRY_DUMPS_V1_AGGREGATE_QUERY,
  FETCH_SCHEMA_REGISTRY_DUMPS_V2_QUERY,
  FETCH_SCHEMA_REGISTRY_DUMPS_V1_QUERY,
} from '../queries';
import {
  GetSchemaRegstiryDumpsV2ResponseWithError,
  GetSchemaRegstiryDumpsV1ResponseWithError,
  SchemaRegistryDumpWithSiblingSchema,
  GetSchemaRegstiryDumpsV1AggregateResponseWithError,
} from '../types';
import {
  FETCH_SCHEMA_REGISTRY_DUMPS_V2_QUERY_NAME,
  FETCH_SCHEMA_REGISTRY_DUMPS_V1_QUERY_NAME,
  SCHEMA_REGISTRY_REFRESH_TIME,
  SCHEMA_LIST_FETCH_BATCH_SIZE,
  FETCH_SCHEMA_REGISTRY_DUMPS_V1_AGGREGATE_QUERY_NAME,
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
      response: SchemaRegistryDumpWithSiblingSchema[];
      totalCount: number;
    };

export const useGetSchemaRegistryList = (
  projectId: string,
  pageNumber: number,
  v2Count: number,
  lastV2EntryCursor: string
): GetSchemaRegistryListResponse => {
  const [v1Count, setV1Count] = useState<number>(0);

  const [v2Dumps, setV2Dumps] = useState<SchemaRegistryDumpWithSiblingSchema[]>(
    []
  );
  const [v1Dumps, setV1Dumps] = useState<SchemaRegistryDumpWithSiblingSchema[]>(
    []
  );

  const [returnedDumps, setReturnedDumps] = useState<
    SchemaRegistryDumpWithSiblingSchema[]
  >([]);

  /**
   * Fetch the aggregate count for Schema Registry Dumps V1 based on last V2 entry
   */
  const fetchSchemaRegistryDumpsV1AggregateFn = (projectId: string) => {
    return schemaRegsitryControlPlaneClient.query<
      GetSchemaRegstiryDumpsV1AggregateResponseWithError,
      { projectId: string; lastV2EntryCursor: string }
    >(FETCH_SCHEMA_REGISTRY_DUMPS_V1_AGGREGATE_QUERY, {
      projectId: projectId,
      lastV2EntryCursor: lastV2EntryCursor,
    });
  };

  const {
    data: v1AggregateData,
    error: v1AggregateError,
    isLoading: v1AggregateLoading,
    refetch,
  } = useQuery({
    queryKey: FETCH_SCHEMA_REGISTRY_DUMPS_V1_AGGREGATE_QUERY_NAME,
    queryFn: () => fetchSchemaRegistryDumpsV1AggregateFn(projectId),
    refetchOnMount: 'always',
    refetchOnWindowFocus: true,
    staleTime: SCHEMA_REGISTRY_REFRESH_TIME,
    onSuccess: response => {
      if (
        response &&
        response.data &&
        response.data.schema_registry_dumps_aggregate?.aggregate?.count
      ) {
        const totalV1Dumps =
          response.data.schema_registry_dumps_aggregate.aggregate.count;
        setV1Count(totalV1Dumps);
      }
    },
  });
  React.useEffect(() => {
    refetch();
  }, [lastV2EntryCursor]);

  const lastV2Page = Math.floor(v2Count / SCHEMA_LIST_FETCH_BATCH_SIZE);

  const v1PageNumber = pageNumber - lastV2Page;

  const v1Offset =
    v1PageNumber > 0
      ? (v1PageNumber - 1) * SCHEMA_LIST_FETCH_BATCH_SIZE +
        (SCHEMA_LIST_FETCH_BATCH_SIZE -
          (v2Count % SCHEMA_LIST_FETCH_BATCH_SIZE))
      : 0;

  const v2Offset = pageNumber * SCHEMA_LIST_FETCH_BATCH_SIZE;

  /**
   * Fetch Schema Registry V2 dumps based on the page number
   * Query gets fired only when aggregate data is fetched and lastV2Page >= pageNumber
   */
  const fetchSchemaRegistryDumpsV2Fn = (
    projectId: string,
    limit: number,
    offset: number
  ) => {
    return schemaRegsitryControlPlaneClient.query<
      GetSchemaRegstiryDumpsV2ResponseWithError,
      { projectId: string; limit: number; offset: number }
    >(FETCH_SCHEMA_REGISTRY_DUMPS_V2_QUERY, {
      projectId: projectId,
      limit: limit,
      offset: offset,
    });
  };

  const {
    data: v2Data,
    error: v2Error,
    isLoading: v2Loading,
    refetch: v2Refetch,
  } = useQuery({
    queryKey: [FETCH_SCHEMA_REGISTRY_DUMPS_V2_QUERY_NAME, pageNumber],
    queryFn: () =>
      fetchSchemaRegistryDumpsV2Fn(
        projectId,
        SCHEMA_LIST_FETCH_BATCH_SIZE,
        v2Offset
      ),
    enabled: lastV2Page >= pageNumber,
    refetchOnMount: 'always',
    refetchOnWindowFocus: true,
    onSuccess: response => {
      if (response && response.data && response.data.schema_registry_dumps_v2) {
        const v2Dumps = response.data.schema_registry_dumps_v2;
        setV2Dumps(v2Dumps);
      }
    },
  });

  React.useEffect(() => {
    if (lastV2Page <= pageNumber) {
      v2Refetch();
    }
    if (v1PageNumber >= 0) {
      v1Refetch();
    }
  }, [pageNumber, lastV2EntryCursor, v1Offset]);

  /**
   * Fetch Schema Registry V1 dumps based on the page number
   * Query gets fired only if change_recorded_at is fetched from V2 and (v1PageNumber >= 0 && v1Offset >= 0)
   */
  const fetchSchemaRegistryDumpsV1Fn = (
    projectId: string,
    limit: number,
    offset: number,
    changeTimestamp: string
  ) => {
    return schemaRegsitryControlPlaneClient.query<
      GetSchemaRegstiryDumpsV1ResponseWithError,
      {
        projectId: string;
        limit: number;
        offset: number;
        changeTimestamp: string;
      }
    >(FETCH_SCHEMA_REGISTRY_DUMPS_V1_QUERY, {
      projectId: projectId,
      limit: limit,
      offset: offset,
      changeTimestamp: changeTimestamp,
    });
  };

  const {
    data: v1Data,
    error: v1Error,
    isLoading: v1Loading,
    refetch: v1Refetch,
  } = useQuery({
    queryKey: [
      FETCH_SCHEMA_REGISTRY_DUMPS_V1_QUERY_NAME,
      v1Offset,
      v1PageNumber,
    ],
    queryFn: () =>
      fetchSchemaRegistryDumpsV1Fn(
        projectId,
        SCHEMA_LIST_FETCH_BATCH_SIZE,
        v1Offset,
        lastV2EntryCursor
      ),
    enabled: v1PageNumber >= 0 && v1Offset >= 0,
    refetchOnMount: 'always',
    refetchOnWindowFocus: true,
    onSuccess: response => {
      if (response && response.data && response.data.schema_registry_dumps) {
        const v1Dumps = response.data.schema_registry_dumps;
        setV1Dumps(v1Dumps);
      }
    },
  });

  React.useEffect(() => {
    if (pageNumber === lastV2Page) {
      // Slicing the results to SCHEMA_LIST_FETCH_BATCH_SIZE as extra v1Dumps might be present
      setReturnedDumps(
        [...v2Dumps, ...v1Dumps].slice(0, SCHEMA_LIST_FETCH_BATCH_SIZE)
      );
    } else if (pageNumber < lastV2Page) {
      setReturnedDumps(v2Dumps);
    } else {
      setReturnedDumps(v1Dumps);
    }
  }, [pageNumber, v1Dumps, v2Dumps]);

  if (v1AggregateLoading || v2Loading || v1Loading) {
    return {
      kind: 'loading',
    };
  }

  if (
    v1AggregateError ||
    !!v1AggregateData?.errors ||
    v2Error ||
    !!v2Data?.errors ||
    v1Error ||
    !!v1Data?.errors
  ) {
    return {
      kind: 'error',
      message: 'error',
    };
  }

  return {
    kind: 'success',
    response: returnedDumps,
    totalCount: v2Count + v1Count,
  };
};
