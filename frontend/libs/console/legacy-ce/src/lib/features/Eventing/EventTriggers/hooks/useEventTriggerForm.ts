import { useState } from 'react';
import {
  defaultState,
  EventTriggerAutoCleanup,
  EventTriggerOperation,
  LocalEventTriggerState,
  RetryConf,
  URLConf,
} from '../types';
import { ClientHeader, Table } from '@hasura/shared/types';

const useEventTriggerForm = (initState?: LocalEventTriggerState) => {
  const [state, setState] = useState(initState || defaultState);
  return {
    state,
    setState: {
      name: (name: string) => {
        setState((s) => ({
          ...s,
          name,
        }));
      },
      source: (chosenSource: string) => {
        setState((s) => ({
          ...s,
          source: chosenSource,
        }));
      },
      table: (table: Table | null) => {
        setState((s) => {
          return {
            ...s,
            table,
          };
        });
      },
      operations: (operations: EventTriggerOperation[]) => {
        setState((s) => ({
          ...s,
          operations,
        }));
      },
      webhook: (webhook: URLConf) => {
        setState((s) => {
          return {
            ...s,
            webhook,
          };
        });
      },
      retryConf: (r: RetryConf) => {
        setState((s) => ({
          ...s,
          retryConf: r,
        }));
      },
      headers: (headers: ClientHeader[]) => {
        setState((s) => ({
          ...s,
          headers,
        }));
      },
      operationColumns: (columns: string[]) => {
        setState((s) => ({
          ...s,
          operationColumns: columns,
        }));
      },
      bulk: (s: LocalEventTriggerState) => {
        setState(s);
      },
      toggleAllColumnChecked: (value: boolean) => {
        setState((s) => ({
          ...s,
          isAllColumnChecked: value,
        }));
      },
      cleanupConfig: (cleanupConfig: EventTriggerAutoCleanup) => {
        setState((s) => ({
          ...s,
          cleanupConfig,
        }));
      },
    },
  };
};

export default useEventTriggerForm;
