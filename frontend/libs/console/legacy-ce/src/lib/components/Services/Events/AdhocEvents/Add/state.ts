import React from 'react';
import { Header, defaultHeader } from '../../../../Common/Headers/Headers';
import { RetryConf } from '../../types';
import { Nullable } from '../../../../Common/utils/tsUtils';

export type LocalAdhocEventState = {
  // name: string,
  webhook: string;
  time: Date;
  payload: string;
  headers: Header[];
  retryConf: RetryConf;
  comment: Nullable<string>;
  loading: boolean;
};

const defaultState: LocalAdhocEventState = {
  webhook: '',
  time: new Date(),
  payload: '{}',
  headers: [defaultHeader],
  retryConf: {
    num_retries: 0,
    timeout_sec: 60,
    interval_sec: 10,
  },
  comment: '',
  loading: false,
};

export const useAdhocEventAdd = () => {
  const [state, setState] = React.useState(defaultState);
  return {
    state,
    setState: {
      webhook: (webhook: string) => setState(s => ({ ...s, webhook })),
      time: (time: Date) => setState(s => ({ ...s, time })),
      payload: (payload: string) => setState(s => ({ ...s, payload })),
      headers: (headers: Header[]) => setState(s => ({ ...s, headers })),
      retryConf: (retryConf: RetryConf) => setState(s => ({ ...s, retryConf })),
      comment: (comment: string) => setState(s => ({ ...s, comment })),
      loading: (isLoading: boolean) =>
        setState(s => ({ ...s, loading: isLoading })),
      bulk: (newState?: LocalAdhocEventState) =>
        setState(newState || defaultState),
    },
  };
};
