import { CronTrigger } from '@hasura/shared/types';
import { useDefaultValues } from './useDefaultValues';
import { emptyDefaultValues } from './utils';

// `useDefaultValues` reads no React state/hooks — it is a pure mapper from a
// server CronTrigger to the form's default values — so it can be unit tested
// directly without rendering.

describe('CronTriggers useDefaultValues', () => {
  it('returns the empty defaults when creating a new trigger', () => {
    expect(useDefaultValues({ currentTrigger: undefined })).toEqual({
      data: emptyDefaultValues,
    });
  });

  describe('when editing an existing trigger', () => {
    const currentTrigger: CronTrigger = {
      name: 'nightly',
      webhook: 'http://hook.test',
      schedule: '0 0 * * *',
      payload: { foo: 'bar' },
      headers: [{ name: 'x-key', value: 'secret' }],
      retry_conf: {
        num_retries: 5,
        retry_interval_seconds: 30,
        timeout_seconds: 120,
        tolerance_seconds: 3600,
      },
      include_in_metadata: true,
      comment: 'runs nightly',
    };

    it('maps the core fields onto the form schema', () => {
      const { data } = useDefaultValues({ currentTrigger });

      expect(data.name).toBe('nightly');
      expect(data.webhook).toBe('http://hook.test');
      expect(data.schedule).toBe('0 0 * * *');
      expect(data.include_in_metadata).toBe(true);
      expect(data.comment).toBe('runs nightly');
    });

    it('serialises the payload to a JSON string', () => {
      const { data } = useDefaultValues({ currentTrigger });
      expect(data.payload).toBe(JSON.stringify({ foo: 'bar' }));
    });

    it('parses server headers into client headers', () => {
      const { data } = useDefaultValues({ currentTrigger });
      expect(data.headers).toEqual([
        { name: 'x-key', value: 'secret', type: 'value' },
      ]);
    });

    it('stringifies the retry configuration', () => {
      const { data } = useDefaultValues({ currentTrigger });
      expect(data.num_retries).toBe('5');
      expect(data.retry_interval_seconds).toBe('30');
      expect(data.timeout_seconds).toBe('120');
      expect(data.tolerance_seconds).toBe('3600');
    });

    it('falls back to retry defaults when retry_conf is missing', () => {
      const { data } = useDefaultValues({
        currentTrigger: { ...currentTrigger, retry_conf: undefined },
      });
      expect(data.num_retries).toBe('0');
      expect(data.retry_interval_seconds).toBe('10');
      expect(data.timeout_seconds).toBe('60');
      expect(data.tolerance_seconds).toBe('21600');
    });

    it('falls back to an empty comment when none is set', () => {
      const { data } = useDefaultValues({
        currentTrigger: { ...currentTrigger, comment: undefined },
      });
      expect(data.comment).toBe('');
    });

    it('surfaces the request transform alongside the form data', () => {
      const request_transform = { version: 2 } as never;
      const result = useDefaultValues({
        currentTrigger: { ...currentTrigger, request_transform },
      });
      expect(result.requestTransform).toBe(request_transform);
    });
  });
});
