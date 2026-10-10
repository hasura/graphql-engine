import { convertDateTimeToLocale } from '@hasura/shared/utils';
import { sanitiseRow } from './utils';

describe('sanitiseRow', () => {
  it('returns the raw cell value for a regular column', () => {
    const row = { status: 'delivered' };
    expect(sanitiseRow('status', row)).toBe('delivered');
  });

  it('returns "NULL" when the value is null', () => {
    const row = { comment: null } as unknown as Record<string, string>;
    expect(sanitiseRow('comment', row)).toBe('NULL');
  });

  it('returns "NULL" when the value is undefined (missing column)', () => {
    const row: Record<string, string> = {};
    expect(sanitiseRow('missing', row)).toBe('NULL');
  });

  it('does not coerce an empty string to NULL', () => {
    const row = { comment: '' };
    expect(sanitiseRow('comment', row)).toBe('');
  });

  it('localises the created_at column via convertDateTimeToLocale', () => {
    const timestamp = '2024-01-01T12:00:00Z';
    const row = { created_at: timestamp };
    expect(sanitiseRow('created_at', row)).toBe(
      convertDateTimeToLocale(timestamp),
    );
  });

  it('localises the scheduled_time column via convertDateTimeToLocale', () => {
    const timestamp = '2024-06-15T08:30:00Z';
    const row = { scheduled_time: timestamp };
    expect(sanitiseRow('scheduled_time', row)).toBe(
      convertDateTimeToLocale(timestamp),
    );
  });
});
