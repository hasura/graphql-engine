import {
  getCronTriggerCreateQuery,
  getCronTriggerDeleteQuery,
  getCronTriggerUpdateQuery,
} from './utils';
import { Schema } from './schema';

const baseValues: Schema = {
  name: 'my_cron',
  webhook: 'http://example.com/hook',
  schedule: '* * * * *',
  payload: '{"hello":"world"}',
  headers: [{ name: 'x-key', value: 'secret', type: 'value' }],
  num_retries: '3',
  retry_interval_seconds: '20',
  timeout_seconds: '90',
  tolerance_seconds: '3600',
  include_in_metadata: true,
  comment: 'a comment',
};

describe('getCronTriggerCreateQuery', () => {
  it('builds a create_cron_trigger payload from the form values', () => {
    const query = getCronTriggerCreateQuery(baseValues);

    expect(query.type).toBe('create_cron_trigger');
    expect(query.args).toMatchObject({
      name: 'my_cron',
      webhook: 'http://example.com/hook',
      schedule: '* * * * *',
      include_in_metadata: true,
      comment: 'a comment',
    });
  });

  it('parses a JSON string payload into an object', () => {
    const query = getCronTriggerCreateQuery(baseValues);
    expect(query.args.payload).toEqual({ hello: 'world' });
  });

  it('keeps a non-JSON payload as a raw string', () => {
    const query = getCronTriggerCreateQuery({
      ...baseValues,
      payload: 'not-json',
    });
    expect(query.args.payload).toBe('not-json');
  });

  it('coerces the retry configuration strings into numbers', () => {
    const query = getCronTriggerCreateQuery(baseValues);
    expect(query.args.retry_conf).toEqual({
      num_retries: 3,
      retry_interval_seconds: 20,
      timeout_seconds: 90,
      tolerance_seconds: 3600,
    });
  });

  it('transforms client headers into the metadata header config', () => {
    const query = getCronTriggerCreateQuery(baseValues);
    expect(query.args.headers).toEqual([{ name: 'x-key', value: 'secret' }]);
  });

  it('does not set replace by default', () => {
    const query = getCronTriggerCreateQuery(baseValues);
    expect(query.args).not.toHaveProperty('replace');
  });

  it('sets replace when requested', () => {
    const query = getCronTriggerCreateQuery(baseValues, undefined, true);
    expect(query.args.replace).toBe(true);
  });

  it('includes the request transform when provided', () => {
    const requestTransform = { version: 2 } as never;
    const query = getCronTriggerCreateQuery(baseValues, requestTransform);
    expect(query.args.request_transform).toBe(requestTransform);
  });
});

describe('getCronTriggerDeleteQuery', () => {
  it('builds a delete_cron_trigger payload for the given name', () => {
    expect(getCronTriggerDeleteQuery('my_cron')).toEqual({
      type: 'delete_cron_trigger',
      args: { name: 'my_cron' },
    });
  });
});

describe('getCronTriggerUpdateQuery', () => {
  it('emits a single replace create when the name is unchanged', () => {
    const query = getCronTriggerUpdateQuery('my_cron', baseValues);

    expect(query.type).toBe('bulk');
    expect(query.args).toHaveLength(1);
    expect(query.args[0].type).toBe('create_cron_trigger');
    expect(query.args[0].args).toMatchObject({ replace: true });
  });

  it('deletes the old trigger and creates the new one when renamed', () => {
    const query = getCronTriggerUpdateQuery('old_name', baseValues);

    expect(query.type).toBe('bulk');
    expect(query.args).toHaveLength(2);
    expect(query.args[0]).toEqual({
      type: 'delete_cron_trigger',
      args: { name: 'old_name' },
    });
    expect(query.args[1].type).toBe('create_cron_trigger');
    // the recreated trigger is not a replace
    expect(query.args[1].args).not.toHaveProperty('replace');
  });
});
