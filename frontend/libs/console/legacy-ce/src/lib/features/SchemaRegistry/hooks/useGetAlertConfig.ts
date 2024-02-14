import * as React from 'react';
import { useQuery } from 'react-query';
import { FETCH_ALERT_CONFIG } from '../queries';
import { GetAlertConfigResponseWithError, AlertType } from '../types';
import { FETCH_ALERT_CONFIG_QUERY_NAME } from '../constants';
import { controlPlaneClient } from '../../ControlPlane';

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

export const useGetAlertConfig = (
  projectId: string,
  type: AlertType
): FetchAlertResponse => {
  const fetchAlertConfigQueryFn = (projectId: string) => {
    return controlPlaneClient.query<
      GetAlertConfigResponseWithError,
      { projectId: string; type: AlertType }
    >(FETCH_ALERT_CONFIG, {
      projectId: projectId,
      type: type,
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
