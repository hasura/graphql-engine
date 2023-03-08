import React from 'react';
import {
  URLConf,
  EventTriggerOperation,
  ETOperationColumn,
  EventTrigger,
  RetryConf,
  DatabaseInfo,
  EventTriggerAutoCleanup,
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
  cleanupConfig?: EventTriggerAutoCleanup;
};

export const defaultState: LocalEventTriggerState = {
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
) => {
  if (!eventTrigger) return defaultState;

  const etConf = eventTrigger.configuration;
  const etDef = etConf.definition;

  const etTableDef = generateTableDef(
    eventTrigger.table_name,
    eventTrigger.schema_name
  );

  const result: LocalEventTriggerState = {
    table: etTableDef,
    operationColumns: [],
    name: eventTrigger.name,
    retryConf: etConf.retry_conf,
    source: eventTrigger.source ?? '',
    operations: parseEventTriggerOperations(etDef),
    isAllColumnChecked: etDef?.update?.columns === '*',
    headers: parseServerHeaders(eventTrigger.configuration.headers),
    webhook: parseServerWebhook(etConf.webhook, etConf.webhook_from_env),
  };

  if (databaseInfo) {
    const columnInfo =
      databaseInfo?.[eventTrigger.schema_name]?.[eventTrigger.table_name] ?? [];

    result.operationColumns = getETOperationColumns(
      etDef.update ? etDef.update.columns : [],
      columnInfo
    );
  }

  if (eventTrigger?.configuration?.cleanup_config) {
    result.cleanupConfig = eventTrigger.configuration.cleanup_config;
  }

  return result;
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
      cleanupConfig: (cleanupConfig: EventTriggerAutoCleanup) => {
        setState(s => ({
          ...s,
          cleanupConfig,
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
