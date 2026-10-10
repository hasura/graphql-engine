import { stringifyNumberValue, emptyDefaultValues } from './utils';

describe('stringifyNumberValue', () => {
  it('stringifies a positive number', () => {
    expect(stringifyNumberValue(42, '10')).toBe('42');
  });

  it('falls back to the default for undefined', () => {
    expect(stringifyNumberValue(undefined, '10')).toBe('10');
  });

  it('falls back to the default for 0 (falsy)', () => {
    // 0 is falsy, so the helper returns the provided default rather than "0"
    expect(stringifyNumberValue(0, '60')).toBe('60');
  });

  it('stringifies negative numbers', () => {
    expect(stringifyNumberValue(-5, '0')).toBe('-5');
  });
});

describe('emptyDefaultValues', () => {
  it('provides sensible empty defaults for a new cron trigger form', () => {
    expect(emptyDefaultValues).toEqual({
      name: '',
      webhook: '',
      schedule: '',
      payload: '',
      headers: [],
      num_retries: '0',
      retry_interval_seconds: '10',
      timeout_seconds: '60',
      tolerance_seconds: '21600',
      include_in_metadata: true,
      comment: '',
    });
  });

  it('exposes retry values as strings (matching the form schema)', () => {
    expect(typeof emptyDefaultValues.num_retries).toBe('string');
    expect(typeof emptyDefaultValues.retry_interval_seconds).toBe('string');
    expect(typeof emptyDefaultValues.timeout_seconds).toBe('string');
    expect(typeof emptyDefaultValues.tolerance_seconds).toBe('string');
  });
});
