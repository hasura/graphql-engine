import React from 'react';
import { TableDefinition } from '../../../../Common/utils/v1QueryUtils';
import {
  URLConf,
  URLType,
  EventTriggerOperation,
  ETOperationColumn,
} from '../../Types';
import { Header, defaultHeader } from '../../../../Common/Headers/Headers';

export type LocalEventTriggerState = {
  name: string;
  table: TableDefinition;
  operations: Record<EventTriggerOperation, boolean>;
  operationColumns: ETOperationColumn[];
  webhook: URLConf;
  retryConf: {
    num_retries: number;
    interval_sec: number;
    timeout_sec: number;
  };
  headers: Header[];
};

const defaultState: LocalEventTriggerState = {
  name: '',
  table: {
    name: '',
    schema: 'public',
  },
  operations: {
    insert: false,
    update: false,
    delete: false,
    manual: false,
  },
  operationColumns: [],
  webhook: {
    type: 'static',
    value: '',
  },
  retryConf: {
    num_retries: 0,
    interval_sec: 10,
    timeout_sec: 60,
  },
  headers: [defaultHeader],
};

export const useEventTriggerAdd = (initState?: LocalEventTriggerState) => {
  const [state, setState] = React.useState(initState || defaultState);
  return {
    state,
    setState: {
      name: (name: string) => {
        setState(s => ({
          ...s,
          name,
        }));
      },
      table: (tableName?: string, schemaName?: string) => {
        setState(s => {
          let newTableDef = { ...s.table };
          if (schemaName && schemaName !== newTableDef.schema) {
            newTableDef = {
              name: '',
              schema: schemaName,
            };
          } else if (tableName) {
            newTableDef = {
              ...newTableDef,
              name: tableName,
            };
          }
          return {
            ...s,
            table: newTableDef,
          };
        });
      },
      operation: (operation: EventTriggerOperation, enabled: boolean) => {
        setState(s => ({
          ...s,
          operations: {
            ...s.operations,
            [operation]: enabled,
          },
        }));
      },
      webhook: (value = '', type?: URLType) => {
        setState(s => {
          let newWhConf = { ...s.webhook };
          if (type && type !== newWhConf.type) {
            newWhConf = {
              value: '',
              type,
            };
          } else {
            newWhConf = {
              ...newWhConf,
              value,
            };
          }
          return {
            ...s,
            webhook: newWhConf,
          };
        });
      },
      retryConf: (
        property: 'num_retries' | 'timeout_sec' | 'interval_sec',
        value: number
      ) => {
        setState(s => ({
          ...s,
          retryConf: {
            ...s.retryConf,
            [property]: value,
          },
        }));
      },
      headers: (headers: Array<Header>) => {
        setState(s => ({
          ...s,
          headers,
        }));
      },
      operationColumns: (columns: ETOperationColumn[]) => {
        setState(s => ({
          ...s,
          operationColumns: columns,
        }));
      },
    },
  };
};

export default defaultState;
