import { hasuraToast } from '@hasura/shared/ui';

// NOTE: Raw SQL is disabled for Cockroach due to this https://github.com/hasura/graphql-engine/issues/8794
export const unsupportedRawSQLDrivers = ['cockroach'];

/** Seconds from the timeout input, where 0 means no timeout. */
export const parseStatementTimeout = (value: string): number => {
  const timeoutInSeconds = Number(value.trim());
  return Number.isFinite(timeoutInSeconds) && timeoutInSeconds > 0
    ? timeoutInSeconds
    : 0;
};

export const checkChangeLang = (
  sql: string,
  selectedDriver: string | null | undefined,
): boolean => {
  return (
    selectedDriver === 'citus' && !sql?.match(/(?:\$\$\s+)?language\s+plpgsql/i)
  );
};

export const checkTextLength = (sql) => {
  const LIMIT = 5000;
  if (sql.length > LIMIT) {
    hasuraToast({
      type: 'warning',
      title: 'SQL query wont be saved in local storage',
      message: `Only SQL queries with less than ${LIMIT} characters will be saved.`,
    });
    return false;
  }

  return true;
};
