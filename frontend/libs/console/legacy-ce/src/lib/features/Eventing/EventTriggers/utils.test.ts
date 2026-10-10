import { EventTrigger, EventTriggerDefinition } from '@hasura/shared/types';
import { TableColumn } from '@hasura/metadata/data-source';
import {
  validateETState,
  validateEventTriggerState,
  parseServerWebhook,
  parseEventTriggerOperations,
  getETOperationColumns,
  parseServerETDefinition,
  getEventRequestSampleInput,
} from './utils';
import { LocalEventTriggerState } from './types';

const column = (name: string, consoleDataType = 'text'): TableColumn =>
  ({ name, consoleDataType }) as TableColumn;

describe('parseServerWebhook', () => {
  it('marks a plain webhook value as static', () => {
    expect(parseServerWebhook('http://hook.test')).toEqual({
      value: 'http://hook.test',
      type: 'static',
    });
  });

  it('marks a webhook-from-env value as env', () => {
    expect(parseServerWebhook(undefined, 'MY_ENV')).toEqual({
      value: 'MY_ENV',
      type: 'env',
    });
  });

  it('prefers the static webhook when both are provided', () => {
    // type is env whenever webhookFromEnv is truthy, but value falls back to
    // the static webhook first
    expect(parseServerWebhook('http://hook.test', 'MY_ENV')).toEqual({
      value: 'http://hook.test',
      type: 'env',
    });
  });

  it('returns an empty static webhook when neither is provided', () => {
    expect(parseServerWebhook(null, null)).toEqual({
      value: '',
      type: 'static',
    });
  });
});

describe('parseEventTriggerOperations', () => {
  it('returns only the enabled operations in a stable order', () => {
    const def: EventTriggerDefinition = {
      enable_manual: true,
      insert: { columns: '*' },
      delete: { columns: '*' },
    };
    expect(parseEventTriggerOperations(def)).toEqual([
      'INSERT',
      'DELETE',
      'MANUAL',
    ]);
  });

  it('returns an empty array when nothing is enabled', () => {
    const def: EventTriggerDefinition = { enable_manual: false };
    expect(parseEventTriggerOperations(def)).toEqual([]);
  });

  it('includes UPDATE when the update spec is present', () => {
    const def: EventTriggerDefinition = {
      enable_manual: false,
      update: { columns: ['a'] },
    };
    expect(parseEventTriggerOperations(def)).toEqual(['UPDATE']);
  });
});

describe('getETOperationColumns', () => {
  const columns = [column('id'), column('name'), column('email')];

  it('returns every column name when update columns is "*"', () => {
    expect(getETOperationColumns('*', columns)).toEqual([
      'id',
      'name',
      'email',
    ]);
  });

  it('returns the update columns as-is when a specific list is given', () => {
    expect(getETOperationColumns(['id', 'name'], columns)).toEqual([
      'id',
      'name',
    ]);
  });

  it('returns an empty array when column info is not an array', () => {
    expect(
      getETOperationColumns('*', undefined as unknown as TableColumn[]),
    ).toEqual([]);
  });
});

describe('parseServerETDefinition', () => {
  const table = { name: 'users', schema: 'public' };
  const columns = [column('id'), column('name')];
  const eventTrigger: EventTrigger = {
    name: 'my_trigger',
    definition: {
      enable_manual: true,
      insert: { columns: '*' },
      update: { columns: '*' },
    },
    retry_conf: { num_retries: 5, interval_sec: 10, timeout_sec: 60 },
    webhook: 'http://hook.test',
    headers: [{ name: 'x-key', value: 'secret' }],
  };

  it('maps the trigger name, table and source', () => {
    const result = parseServerETDefinition({
      eventTrigger,
      table,
      columns,
      source: 'default',
    });

    expect(result.name).toBe('my_trigger');
    expect(result.table).toEqual(table);
    expect(result.source).toBe('default');
  });

  it('derives the operations list from the definition', () => {
    const result = parseServerETDefinition({
      eventTrigger,
      table,
      columns,
      source: 'default',
    });

    expect(result.operations).toEqual(['INSERT', 'UPDATE', 'MANUAL']);
  });

  it('flags isAllColumnChecked when update columns is "*" and expands the columns', () => {
    const result = parseServerETDefinition({
      eventTrigger,
      table,
      columns,
      source: 'default',
    });

    expect(result.isAllColumnChecked).toBe(true);
    expect(result.operationColumns).toEqual(['id', 'name']);
  });

  it('parses the webhook into a static URLConf', () => {
    const result = parseServerETDefinition({
      eventTrigger,
      table,
      columns,
      source: 'default',
    });

    expect(result.webhook).toEqual({
      value: 'http://hook.test',
      type: 'static',
    });
  });
});

describe('validateETState', () => {
  const validState: LocalEventTriggerState = {
    name: 'trigger',
    table: { name: 'users', schema: 'public' },
    operations: ['INSERT'],
    operationColumns: [],
    webhook: { type: 'static', value: 'http://hook.test' },
    retryConf: {},
    headers: [],
    source: 'default',
    isAllColumnChecked: true,
  };

  it('returns null for a valid state', () => {
    expect(validateETState(validState)).toBeNull();
  });

  it('rejects a missing trigger name', () => {
    expect(validateETState({ ...validState, name: '' })).toBe(
      'Trigger name cannot be empty',
    );
  });

  it('rejects a missing table', () => {
    expect(validateETState({ ...validState, table: null })).toBe(
      'Table cannot be empty',
    );
  });

  it('rejects an empty webhook value', () => {
    expect(
      validateETState({
        ...validState,
        webhook: { type: 'static', value: '' },
      }),
    ).toBe('Webhook URL cannot be empty');
  });

  it('rejects an invalid static webhook URL', () => {
    expect(
      validateETState({
        ...validState,
        webhook: { type: 'static', value: 'not a url' },
      }),
    ).toBe('Invalid webhook URL');
  });

  it('accepts a templated static webhook URL', () => {
    expect(
      validateETState({
        ...validState,
        webhook: { type: 'static', value: '{{TEMPLATE}}/hook' },
      }),
    ).toBeNull();
  });

  it('does not URL-validate env webhooks', () => {
    expect(
      validateETState({
        ...validState,
        webhook: { type: 'env', value: 'MY_ENV_VAR' },
      }),
    ).toBeNull();
  });
});

describe('validateEventTriggerState', () => {
  const base: LocalEventTriggerState = {
    name: 'trigger',
    table: { name: 'users', schema: 'public' },
    operations: ['INSERT'],
    operationColumns: [],
    webhook: { type: 'static', value: 'http://hook.test' },
    retryConf: {},
    headers: [],
    source: 'default',
    isAllColumnChecked: false,
  };

  it('returns an empty string for a valid state', () => {
    expect(validateEventTriggerState(base)).toBe('');
  });

  it('requires at least one operation', () => {
    expect(validateEventTriggerState({ ...base, operations: [] })).toBe(
      'Please select at-least one operation.',
    );
  });

  it('requires update columns when UPDATE is selected without "all columns"', () => {
    expect(
      validateEventTriggerState({
        ...base,
        operations: ['UPDATE'],
        operationColumns: [],
        isAllColumnChecked: false,
      }),
    ).toBe(
      'Please select at-least one trigger column for the update trigger operation.',
    );
  });

  it('allows UPDATE with all columns checked and no explicit columns', () => {
    expect(
      validateEventTriggerState({
        ...base,
        operations: ['UPDATE'],
        operationColumns: [],
        isAllColumnChecked: true,
      }),
    ).toBe('');
  });

  it('allows UPDATE with explicit columns selected', () => {
    expect(
      validateEventTriggerState({
        ...base,
        operations: ['UPDATE'],
        operationColumns: ['id'],
        isAllColumnChecked: false,
      }),
    ).toBe('');
  });
});

describe('getEventRequestSampleInput', () => {
  it('produces a JSON string with sensible defaults', () => {
    const parsed = JSON.parse(getEventRequestSampleInput());

    expect(parsed.trigger).toEqual({ name: 'triggerName' });
    expect(parsed.table).toEqual({ schema: 'schemaName', name: 'tableName' });
    expect(parsed.delivery_info).toEqual({ max_retries: 0, current_retry: 0 });
    expect(parsed.event.op).toBe('');
    expect(parsed.event.data).toEqual({ old: null, new: null });
  });

  it('uses the provided name, table and retry count', () => {
    const parsed = JSON.parse(
      getEventRequestSampleInput('myTrigger', { name: 't', schema: 's' }, 7),
    );

    expect(parsed.trigger.name).toBe('myTrigger');
    expect(parsed.table).toEqual({ name: 't', schema: 's' });
    expect(parsed.delivery_info.max_retries).toBe(7);
  });

  it('populates event.data.new for an INSERT operation', () => {
    const parsed = JSON.parse(
      getEventRequestSampleInput(
        'myTrigger',
        undefined,
        0,
        [column('id')],
        ['INSERT'],
      ),
    );

    expect(parsed.event.op).toBe('INSERT');
    // text columns echo their own name as the sample value
    expect(parsed.event.data.new).toEqual({ id: 'id' });
    expect(parsed.event.data.old).toBeNull();
  });

  it('populates both old and new for an UPDATE operation', () => {
    const parsed = JSON.parse(
      getEventRequestSampleInput(
        'myTrigger',
        undefined,
        0,
        [column('id')],
        ['UPDATE'],
      ),
    );

    expect(parsed.event.op).toBe('UPDATE');
    expect(parsed.event.data.new).toEqual({ id: 'id' });
    expect(parsed.event.data.old).toEqual({ id: 'id' });
  });
});
