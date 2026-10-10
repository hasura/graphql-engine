import { parseServerHeaders } from '@hasura/shared/ui';
import {
  defaultState,
  LocalEventTriggerState,
  EventTriggerOperation,
  URLConf,
} from './types';
import { isURLTemplated, isValidURL } from '@hasura/shared/utils';
import {
  EventTrigger,
  EventTriggerDefinition,
  Table,
} from '@hasura/shared/types';
import {
  requestBodyActionState,
  requestTransformState,
} from '../../ConfigureTransformation/requestTransformState';
import {
  getEnvVarsFromLS,
  getSessionVarsFromLS,
} from '../../ConfigureTransformation/utils';
import { RequestTransformState } from '../../ConfigureTransformation/stateDefaults';
import { TableColumn } from '@hasura/metadata/data-source';

export const validateETState = (state: LocalEventTriggerState) => {
  if (!state.name) {
    return 'Trigger name cannot be empty';
  }
  if (!state.table) {
    return 'Table cannot be empty';
  }
  if (!state.webhook.value) {
    return 'Webhook URL cannot be empty';
  }
  if (
    state.webhook.type === 'static' &&
    !(isValidURL(state.webhook.value) || isURLTemplated(state.webhook.value))
  ) {
    return 'Invalid webhook URL';
  }

  return null;
};

type ColVals = string | number | boolean;

/**
 * Returns an example value to be used in `Sample Input` section of `Rest connectors`
 * We don't need strict checking of all types as its used for sample input
 * @param type Type of the table column, varies with different databases
 * @param value Name of the table column
 */
const getValueFromDataType = (
  typeStr: TableColumn['consoleDataType'],
  value: string,
): ColVals => {
  const maxNum = 20;
  if (typeStr === 'integer' || typeStr === 'float' || typeStr === 'number') {
    return Math.floor(Math.random() * maxNum);
  }

  if (typeStr === 'boolean') {
    return true;
  }

  if (typeStr.includes('date') || typeStr.includes('timestamp')) {
    return new Date().toISOString();
  }

  return value;
};

const getColumnDataFromOpCols = (
  operationColumns: TableColumn[],
): Record<string, ColVals> => {
  return operationColumns.reduce(
    (acc, op) => {
      acc[op.name] = getValueFromDataType(op.consoleDataType, op.name);

      return acc;
    },
    {} as Record<string, ColVals>,
  );
};

const getDataFromOpCols = (
  opValue: EventTriggerOperation,
  operationColumns: TableColumn[],
) => {
  const opData: {
    old: Record<string, ColVals> | null;
    new: Record<string, ColVals> | null;
  } = { old: null, new: null };
  if (opValue === 'INSERT' || opValue === 'UPDATE' || opValue === 'MANUAL') {
    opData.new = getColumnDataFromOpCols(operationColumns);
  }

  if (opValue === 'UPDATE' || opValue === 'DELETE') {
    opData.old = getColumnDataFromOpCols(operationColumns);
  }
  return opData;
};

const getOperationValue = (op: EventTriggerOperation[]) => {
  return op.length ? op[0] : '';
};

export const getEventRequestSampleInput = (
  name?: string,
  table?: Table | null,
  retries?: number,
  operationColumns?: TableColumn[],
  operations?: EventTriggerOperation[],
) => {
  const opValue = operations ? getOperationValue(operations) : '';
  const opData =
    opValue && operationColumns
      ? getDataFromOpCols(opValue, operationColumns)
      : { old: null, new: null };

  const obj = {
    event: {
      op: opValue,
      data: opData,
      trace_context: {
        trace_id: '501ad47ed3570385',
        span_id: 'd586cc98cee55ad1',
      },
    },
    created_at: new Date().toISOString(),
    id: '2c173942-a860-4a4c-ab71-9a29e2384d54',
    delivery_info: { max_retries: retries ?? 0, current_retry: 0 },
    trigger: { name: name ?? 'triggerName' },
    table: table ?? {
      schema: 'schemaName',
      name: 'tableName',
    },
  };

  const value = JSON.stringify(obj, null, 2);
  return value;
};

export const getEventRequestTransformDefaultState =
  (): RequestTransformState => {
    return {
      ...requestTransformState,
      envVars: getEnvVarsFromLS(),
      sessionVars: getSessionVarsFromLS(),
      requestQueryParams: [{ name: '', value: '' }],
      requestAddHeaders: [{ name: '', value: '' }],
      requestBody: {
        action: requestBodyActionState.transformApplicationJson,
        template: `{
  "table": {
    "name": {{$body.table.name}},
    "schema": {{$body.table.schema}}
  }
}`,
        form_template: [{ name: 'name', value: '{{$body.table.name}}' }],
      },
      requestSampleInput: getEventRequestSampleInput(),
    };
  };

/**
 * Validate the local event trigger state.
 * @param state the local event trigger state.
 * @returns error message if the state is invalid.
 */
export const validateEventTriggerState = (
  state: LocalEventTriggerState,
): string => {
  if (!state.operations.length) {
    return 'Please select at-least one operation.';
  }

  if (
    !state.operationColumns.length &&
    state.operations.includes('UPDATE') &&
    !state.isAllColumnChecked
  ) {
    return 'Please select at-least one trigger column for the update trigger operation.';
  }

  return '';
};

export const parseServerETDefinition = ({
  eventTrigger,
  table,
  columns,
  source,
}: {
  eventTrigger: EventTrigger;
  table: Table;
  columns: TableColumn[];
  source: string;
}) => {
  if (!eventTrigger) return defaultState;

  const etDef = eventTrigger.definition;

  const result: LocalEventTriggerState = {
    table: table,
    operationColumns: [],
    name: eventTrigger.name,
    retryConf: eventTrigger.retry_conf,
    source,
    operations: parseEventTriggerOperations(etDef),
    isAllColumnChecked: etDef?.update?.columns === '*',
    headers: parseServerHeaders(eventTrigger.headers),
    webhook: parseServerWebhook(
      eventTrigger.webhook,
      eventTrigger.webhook_from_env,
    ),
    cleanupConfig: eventTrigger.cleanup_config,
  };

  result.operationColumns = getETOperationColumns(
    etDef.update ? etDef.update.columns : [],
    columns,
  );

  return result;
};

export const parseServerWebhook = (
  webhook: string | null | undefined,
  webhookFromEnv?: string | null,
): URLConf => {
  return {
    value: webhook || webhookFromEnv || '',
    type: webhookFromEnv ? 'env' : 'static',
  };
};

export const parseEventTriggerOperations = (
  etDef: EventTriggerDefinition,
): EventTriggerOperation[] => {
  return [
    etDef.insert ? 'INSERT' : '',
    etDef.update ? 'UPDATE' : '',
    etDef.delete ? 'DELETE' : '',
    etDef.enable_manual ? 'MANUAL' : '',
  ].filter(Boolean) as EventTriggerOperation[];
};

export const getETOperationColumns = (
  updateColumns: string[] | '*',
  columnInfo: TableColumn[],
): string[] => {
  if (columnInfo && Array.isArray(columnInfo)) {
    return updateColumns === '*'
      ? columnInfo.map((col) => col.name)
      : updateColumns;
  }

  return [];
};
