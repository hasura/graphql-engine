import {
  eventTriggerState,
  eventTriggerStateWithoutHeaders,
  requestTransform,
} from './fixtures/input';
import { generateCreateEventTriggerQuery } from '../hooks/useCreateEventTrigger';

describe('generateCreateEventTriggerQuery', () => {
  describe('while creating an event trigger (replace = false)', () => {
    it('uses the driver-prefixed create query type and does not replace', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
      );

      expect(res.type).toBe('pg_create_event_trigger');
      expect(res.args.replace).toBe(false);
    });

    it('maps the trigger name, table and source onto the query args', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
      );

      expect(res.args.name).toBe('trigger_name');
      expect(res.args.table).toEqual({ name: 'table_name', schema: 'public' });
      expect(res.args.source).toBe('default');
    });

    it('sends a static webhook as `webhook` and leaves `webhook_from_env` null', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
      );

      expect(res.args.webhook).toBe('http://httpbin.org/post');
      expect(res.args.webhook_from_env).toBeNull();
    });

    it('sends an env webhook as `webhook_from_env` and leaves `webhook` null', () => {
      const res = generateCreateEventTriggerQuery(
        {
          ...eventTriggerState,
          webhook: { type: 'env', value: 'MY_WEBHOOK_ENV' },
        },
        'postgres',
      );

      expect(res.args.webhook).toBeNull();
      expect(res.args.webhook_from_env).toBe('MY_WEBHOOK_ENV');
    });

    it('trims whitespace from the trigger name and webhook value', () => {
      const res = generateCreateEventTriggerQuery(
        {
          ...eventTriggerState,
          name: '  trigger_name  ',
          webhook: { type: 'static', value: '  http://httpbin.org/post  ' },
        },
        'postgres',
      );

      expect(res.args.name).toBe('trigger_name');
      expect(res.args.webhook).toBe('http://httpbin.org/post');
    });

    it('only enables the operations present in state.operations', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
      );

      // fixture only enables INSERT
      expect(res.args.insert).toEqual({ columns: '*' });
      expect(res.args.update).toBeNull();
      expect(res.args.delete).toBeNull();
      expect(res.args.enable_manual).toBe(false);
    });

    it('enables every operation and sets enable_manual when all are selected', () => {
      const res = generateCreateEventTriggerQuery(
        {
          ...eventTriggerState,
          operations: ['INSERT', 'UPDATE', 'DELETE', 'MANUAL'],
        },
        'postgres',
      );

      expect(res.args.insert).toEqual({ columns: '*' });
      expect(res.args.delete).toEqual({ columns: '*' });
      expect(res.args.enable_manual).toBe(true);
      // isAllColumnChecked is true in the fixture
      expect(res.args.update).toEqual({ columns: '*' });
    });

    it('uses the selected update columns when isAllColumnChecked is false', () => {
      const res = generateCreateEventTriggerQuery(
        {
          ...eventTriggerState,
          operations: ['UPDATE'],
          isAllColumnChecked: false,
          operationColumns: ['name', 'id'],
        },
        'postgres',
      );

      expect(res.args.update).toEqual({ columns: ['name', 'id'] });
    });

    it('allows update triggers with no selected columns', () => {
      const res = generateCreateEventTriggerQuery(
        {
          ...eventTriggerStateWithoutHeaders,
          operations: ['UPDATE'],
        },
        'postgres',
      );

      expect(res.args.update?.columns).toEqual([]);
    });

    it('transforms client headers into the metadata header config', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
      );

      expect(res.args.headers).toEqual([{ name: 'key', value: 'value' }]);
    });

    it('emits an empty header list when there are no headers', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerStateWithoutHeaders,
        'postgres',
      );

      expect(res.args.headers).toEqual([]);
    });

    it('passes the retry configuration through untouched', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
      );

      expect(res.args.retry_conf).toEqual({
        num_retries: 10,
        interval_sec: 20,
        timeout_sec: 90,
      });
    });

    it('includes the request transform when one is supplied', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
        false,
        requestTransform,
      );

      expect(res.args.request_transform).toEqual(requestTransform);
      expect(res).toMatchSnapshot();
    });

    it('leaves request_transform undefined when none is supplied', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
      );

      expect(res.args.request_transform).toBeUndefined();
      expect(res).toMatchSnapshot();
    });

    it('omits the cleanup config when state has none', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
      );

      expect(res.args).not.toHaveProperty('cleanup_config');
    });

    it('merges the state cleanup config on top of the defaults', () => {
      const res = generateCreateEventTriggerQuery(
        {
          ...eventTriggerState,
          cleanupConfig: { batch_size: 42, paused: true },
        },
        'postgres',
      );

      expect(res.args).toHaveProperty('cleanup_config');
      expect(res.args.cleanup_config).toMatchObject({
        batch_size: 42,
        paused: true,
      });
    });
  });

  describe('while modifying an event trigger (replace = true)', () => {
    it('sets replace to true', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
        true,
      );

      expect(res.args.replace).toBe(true);
    });

    it('still transforms headers and includes the request transform', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerState,
        'postgres',
        true,
        requestTransform,
      );

      expect(res.args.headers).toEqual([{ name: 'key', value: 'value' }]);
      expect(res.args.request_transform).toEqual(requestTransform);
      expect(res).toMatchSnapshot();
    });

    it('emits an empty header list and no request transform when absent', () => {
      const res = generateCreateEventTriggerQuery(
        eventTriggerStateWithoutHeaders,
        'postgres',
        true,
      );

      expect(res.args.headers).toEqual([]);
      expect(res.args.request_transform).toBeUndefined();
    });
  });
});
