export function logSentryEnabled(environment: string) {
  console.group();
  console.log(
    '%c Sentry Tracing Enabled ',
    'background: #A0D7D1; color: black; display: block;'
  );
  console.log(
    `%c Sentry Environment: ${environment} `,
    'background: #A0D7D1; color: black; display: block;'
  );
  console.groupEnd();
}

export function logSentryDisabled() {
  // Agree with https://github.com/hasura/graphql-engine-mono/pull/5699#issuecomment-1234205492
  // we are not going to log anything to the user until we have a user-facing doc that speaks about
  // Sentry
  // TODO: log the missing/invalid status
  return;

  console.group();
  console.log(
    '%c Sentry Tracing Disabled ',
    'background: #A0D7D1; color: black; display: block;'
  );
  console.log(
    '%c Provide HASURA_CONSOLE_SENTRY_DSN env variable to enable it ',
    'background: #A0D7D1; color: black; display: block;'
  );
  console.groupEnd();
}
