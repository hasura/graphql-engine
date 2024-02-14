import * as React from 'react';
import { useQuery } from 'react-query';
import { FETCH_SLACK_STATE } from '../queries';
import { GetSlackStateResponseWithError } from '../types';
import { FETCH_SLACK_STATE_QUERY_NAME } from '../constants';
import { controlPlaneClient } from '../../ControlPlane';

type FetchSlackStateResponse =
  | {
      kind: 'loading';
    }
  | {
      kind: 'error';
      message: string;
    }
  | {
      kind: 'success';
      response: NonNullable<GetSlackStateResponseWithError['data']>;
    };

export const useGetSlackState = (
  projectId: string
): FetchSlackStateResponse => {
  const fetchSlackStateQueryFn = (projectId: string) => {
    return controlPlaneClient.query<
      GetSlackStateResponseWithError,
      { projectId: string }
    >(FETCH_SLACK_STATE, {
      projectId: projectId,
    });
  };

  const { data, error, isLoading } = useQuery({
    queryKey: FETCH_SLACK_STATE_QUERY_NAME,
    queryFn: () => fetchSlackStateQueryFn(projectId),
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
