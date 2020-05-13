import {
  URLConf,
  EventTriggerOperationDefinition,
  EventTriggerOperation,
  ETOperationColumn,
  EventTrigger,
  ScheduledTrigger,
  EventKind,
} from './Types';
import {
  TableColumn,
  findTable,
  Table,
  generateTableDef,
} from '../../Common/utils/pgUtils';
import { TableDefinition } from '../../Common/utils/v1QueryUtils';
import { convertDateTimeToLocale } from '../../Common/utils/jsUtils';
import { Nullable } from '../../Common/utils/tsUtils';

export const parseServerWebhook = (
  webhook: Nullable<string>,
  webhookFromEnv?: Nullable<string>
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

export const sanitiseRow = (column: string, row: Record<string, string>) => {
  if (column === 'created_at') {
    return convertDateTimeToLocale(row[column], true);
  }
  if (column === 'scheduled_time') {
    return convertDateTimeToLocale(row[column]);
  }
  const content =
    row[column] === undefined || row[column] === null
      ? 'NULL'
      : row[column].toString();
  return content;
};

export const getLogsTableDef = (kind: EventKind): TableDefinition => {
  let tableName: string;
  switch (kind) {
    case 'data':
      tableName = 'event_invocation_logs';
      break;
    case 'cron':
      tableName = 'hdb_cron_event_invocation_logs';
      break;
    case 'scheduled':
      tableName = 'hdb_scheduled_event_invocation_logs';
      break;
    default:
      tableName = 'hdb_scheduled_event_invocation_logs';
      break;
  }
  return generateTableDef(tableName, 'hdb_catalog');
};
