import { getScheduledEventCreateQuery } from './utils';
import { Schema } from './schema';

const baseValues: Schema = {
  webhook: 'http://example.com/hook',
  time: '2024-01-01T00:00:00Z',
  payload: '{"foo":"bar"}',
  headers: [{ name: 'x-key', value: 'secret', type: 'value' }],
  num_retries: 2,
  retry_interval_seconds: 15,
  timeout_seconds: 60,
  comment: 'one off',
};

describe('getScheduledEventCreateQuery', () => {
  it('builds a create_scheduled_event payload from the form values', () => {
    const query = getScheduledEventCreateQuery(baseValues);

    expect(query.type).toBe('create_scheduled_event');
    expect(query.args).toMatchObject({
      webhook: 'http://example.com/hook',
      schedule_at: '2024-01-01T00:00:00Z',
      comment: 'one off',
    });
  });

  it('always includes the event in metadata and leaves the name empty', () => {
    const query = getScheduledEventCreateQuery(baseValues);

    expect(query.args.include_in_metadata).toBe(true);
    expect(query.args.name).toBe('');
  });

  it('parses a JSON string payload into an object', () => {
    const query = getScheduledEventCreateQuery(baseValues);
    expect(query.args.payload).toEqual({ foo: 'bar' });
  });

  it('keeps a non-JSON payload as a raw string', () => {
    const query = getScheduledEventCreateQuery({
      ...baseValues,
      payload: 'plain text',
    });
    expect(query.args.payload).toBe('plain text');
  });

  it('maps the retry configuration (no tolerance for one-off events)', () => {
    const query = getScheduledEventCreateQuery(baseValues);

    expect(query.args.retry_conf).toEqual({
      num_retries: 2,
      retry_interval_seconds: 15,
      timeout_seconds: 60,
    });
    expect(query.args.retry_conf).not.toHaveProperty('tolerance_seconds');
  });

  it('transforms client headers into the metadata header config', () => {
    const query = getScheduledEventCreateQuery(baseValues);
    expect(query.args.headers).toEqual([{ name: 'x-key', value: 'secret' }]);
  });

  it('transforms an env header into a value_from_env config', () => {
    const query = getScheduledEventCreateQuery({
      ...baseValues,
      headers: [{ name: 'x-key', value: 'MY_ENV', type: 'env' }],
    });
    expect(query.args.headers).toEqual([
      { name: 'x-key', value_from_env: 'MY_ENV' },
    ]);
  });
});
