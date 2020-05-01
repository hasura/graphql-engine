import {
  URLConf,
  EventTriggerOperationDefinition,
  EventTriggerOperation,
  ETOperationColumn,
  EventTrigger,
  ScheduledTrigger,
} from './Types';
import {
  TableColumn,
  findTable,
  Table,
  generateTableDef,
} from '../../Common/utils/pgUtils';

export const parseServerWebhook = (
  webhook?: string,
  webhookFromEnv?: string
): URLConf => {
  return {
    value: webhook || webhookFromEnv || '',
    type: webhookFromEnv ? 'env' : 'static',
  };
};

export const parseEventTriggerOperations = (
  etDef: Record<EventTriggerOperation, EventTriggerOperationDefinition>
): Record<EventTriggerOperation, boolean> => {
  return {
    insert: !!etDef.insert,
    update: !!etDef.update,
    delete: !!etDef.delete,
    enable_manual: !!etDef.enable_manual,
  };
};

export const getETOperationColumns = (
  updateColumns: string[] | '*',
  columns: TableColumn[]
): ETOperationColumn[] => {
  return columns.map(c => {
    return {
      name: c.column_name,
      enabled:
        updateColumns === '*' ? true : updateColumns.includes(c.column_name),
      type: c.data_type,
    };
  });
};

export const findETTable = (et: EventTrigger, allTables: Table[] = []) => {
  if (!et) return undefined;
  return findTable(allTables, generateTableDef(et.table_name, et.schema_name));
};

export const findEventTrigger = (
  triggerName: string,
  allTriggers: EventTrigger[]
) => allTriggers.find(t => t.name === triggerName);

export const findScheduledTrigger = (
  triggerName: string,
  allTriggers: ScheduledTrigger[]
) => allTriggers.find(t => t.name === triggerName);
