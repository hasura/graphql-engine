import type { SentryDsn } from './types';

type ParseSentryDsnResult =
  | { status: 'missing' }
  | { status: 'invalid'; value: unknown }
  | { status: 'valid'; value: SentryDsn };

export function parseSentryDsn(value: unknown): ParseSentryDsnResult {
  if (value === '' || value === undefined || value === null) {
    return { status: 'missing' };
  }

  if (typeof value !== 'string') {
    return { status: 'invalid', value };
  }

  if (!isSentryDsn(value)) {
    return { status: 'invalid', value };
  }

  return { status: 'valid', value };
}

function isSentryDsn(url: unknown): url is SentryDsn {
  if (typeof url !== 'string') return false;

  if (!url.startsWith('https://')) return false;

  if (!url.includes('.ingest.sentry.io/')) return false;

  const missSentryProjectId = url.endsWith('.ingest.sentry.io/');
  if (missSentryProjectId) return false;

  const missSentryPublicKey = url.includes('https://.ingest.sentry.io');
  if (missSentryPublicKey) return false;

  return true;
}
