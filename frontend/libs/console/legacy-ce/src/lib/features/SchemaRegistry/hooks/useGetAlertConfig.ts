import * as React from 'react';
import { useQuery } from 'react-query';
import { FETCH_ALERT_CONFIG } from '../queries';
import { schemaRegsitryControlPlaneClient } from '../utils';
import { GetAlertConfigResponseWithError } from '../types';
import { FETCH_ALERT_CONFIG_QUERY_NAME } from '../constants';

type FetchAlertResponse =
  | {
      kind: 'loading';
    }
  | {
      kind: 'error';
      message: string;
    }
  | {
      kind: 'success';
      response: NonNullable<GetAlertConfigResponseWithError['data']>;
    };

export const useGetAlertConfig = (projectId: string): FetchAlertResponse => {
  const fetchAlertConfigQueryFn = (projectId: string) => {
    return schemaRegsitryControlPlaneClient.query<
      GetAlertConfigResponseWithError,
      { projectId: string }
    >(FETCH_ALERT_CONFIG, {
      projectId: projectId,
    });
  };

  const { data, error, isLoading } = useQuery({
    queryKey: FETCH_ALERT_CONFIG_QUERY_NAME,
    queryFn: () => fetchAlertConfigQueryFn(projectId),
    refetchOnMount: true,
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
