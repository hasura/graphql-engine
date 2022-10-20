type SentryPublicKey = string;
type SentryProjectId = string;

// see https://docs.sentry.io/product/sentry-basics/dsn-explainer/
export type SentryDsn =
  `https://${SentryPublicKey}.ingest.sentry.io/${SentryProjectId}`;
