import React from 'react';
import { Header, defaultHeader } from '../../../Common/Headers/Headers';
import { RetryConf } from '../types';
import { Nullable } from '../../../Common/utils/tsUtils';

export const defaultCronExpr = '* * * * *';

export type LocalScheduledTriggerState = {
  name: string;
  webhook: string;
  schedule: string;
  payload: string;
  headers: Header[];
  loading: Record<FormAsyncAction, boolean>;
  retryConf: RetryConf;
  includeInMetadata: boolean;
  comment: Nullable<string>;
};

const defaultState: LocalScheduledTriggerState = {
  name: '',
  webhook: '',
  schedule: '',
  payload: '',
  headers: [defaultHeader],
  loading: {
    modify: false,
    delete: false,
    add: false,
  },
  retryConf: {
    timeout_sec: 60,
    num_retries: 0,
    interval_sec: 10,
  },
  includeInMetadata: true,
  comment: null,
};

type FormAsyncAction = 'modify' | 'delete' | 'add';

export const useScheduledTrigger = (initState?: LocalScheduledTriggerState) => {
  const [state, setState] = React.useState(initState || defaultState);

  return {
    state,
    setState: {
      name: (name: string) => {
        setState(s => ({ ...s, name }));
      },
      webhook: (webhook: string) => {
        setState(s => ({ ...s, webhook }));
      },
      schedule: (schedule: string) => {
        setState(s => ({
          ...s,
          schedule,
        }));
      },
      payload: (jsonString: string) => {
        setState(s => ({
          ...s,
          payload: jsonString,
        }));
      },
      headers: (headers: Header[]) => {
        setState(s => ({
          ...s,
          headers,
        }));
      },
      loading: (action: FormAsyncAction, isLoading: boolean) => {
        setState(s => ({
          ...s,
          loading: {
            ...s.loading,
            [action]: isLoading,
          },
        }));
      },
      retryConf: (r: RetryConf) => {
        setState(s => ({
          ...s,
          retryConf: r,
        }));
      },
      comment: (comment: string) => {
        setState(s => ({
          ...s,
          comment,
        }));
      },
      toggleIncludeInMetadata: () => {
        setState(s => ({
          ...s,
          includeInMetadata: !s.includeInMetadata,
        }));
      },
      bulk: setState,
    },
  };
};
