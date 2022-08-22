import React from 'react';
import {
  URLConf,
  EventTriggerOperation,
  ETOperationColumn,
  EventTrigger,
  RetryConf,
  DatabaseInfo,
} from '../types';
import { Header, defaultHeader } from '../../../Common/Headers/Headers';
import {
  parseServerWebhook,
  parseEventTriggerOperations,
  getETOperationColumns,
} from '../utils';
import { parseServerHeaders } from '../../../Common/Headers/utils';
import { generateTableDef } from '../../../../dataSources';
import { QualifiedTable } from '../../../../metadata/types';

export type LocalEventTriggerState = {
  name: string;
  table: QualifiedTable;
  operations: Record<EventTriggerOperation, boolean>;
  operationColumns: ETOperationColumn[];
  webhook: URLConf;
  retryConf: RetryConf;
  headers: Header[];
  source: string;
  isAllColumnChecked: boolean;
};

const defaultState: LocalEventTriggerState = {
  name: '',
  table: {
    name: '',
    schema: '',
  },
  operations: {
    insert: false,
    update: false,
    delete: false,
    enable_manual: false,
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
  source: '',
  isAllColumnChecked: true,
};

export const parseServerETDefinition = (
  eventTrigger?: EventTrigger,
  databaseInfo?: DatabaseInfo
): LocalEventTriggerState => {
  if (!eventTrigger) {
    return defaultState;
  }

  const etConf = eventTrigger.configuration;
  const etDef = etConf.definition;

  const etTableDef = generateTableDef(
    eventTrigger.table_name,
    eventTrigger.schema_name
  );
  const columnInfo =
    databaseInfo?.[eventTrigger.schema_name]?.[eventTrigger.table_name] ?? [];

  return {
    name: eventTrigger.name,
    source: eventTrigger.source ?? '',
    table: etTableDef,
    operations: parseEventTriggerOperations(etDef),
    operationColumns: databaseInfo
      ? getETOperationColumns(
          etDef.update ? etDef.update.columns : [],
          columnInfo
        )
      : [],
    webhook: parseServerWebhook(etConf.webhook, etConf.webhook_from_env),
    retryConf: etConf.retry_conf,
    headers: parseServerHeaders(eventTrigger.configuration.headers),
    isAllColumnChecked: etDef?.update?.columns === '*',
  };
};

export const useEventTrigger = (initState?: LocalEventTriggerState) => {
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
      source: (chosenSource: string) => {
        setState(s => ({
          ...s,
          source: chosenSource,
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
          } else if (!tableName && !schemaName) {
            newTableDef = {
              name: '',
              schema: '',
            };
          }
          return {
            ...s,
            table: newTableDef,
          };
        });
      },
      operations: (operations: Record<EventTriggerOperation, boolean>) => {
        setState(s => ({
          ...s,
          operations,
        }));
      },
      webhook: (webhook: URLConf) => {
        setState(s => {
          return {
            ...s,
            webhook,
          };
        });
      },
      retryConf: (r: RetryConf) => {
        setState(s => ({
          ...s,
          retryConf: r,
        }));
      },
      headers: (headers: Header[]) => {
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
      bulk: (s: LocalEventTriggerState) => {
        setState(s);
      },
      toggleAllColumnChecked: () => {
        setState(s => ({
          ...s,
          isAllColumnChecked: !s.isAllColumnChecked,
        }));
      },
    },
  };
};

export const useEventTriggerModify = (
  eventTrigger: EventTrigger,
  databaseInfo: DatabaseInfo
) => {
  const { state, setState } = useEventTrigger(
    parseServerETDefinition(eventTrigger, databaseInfo)
  );

  return {
    state,
    setState,
  };
};

export default defaultState;
