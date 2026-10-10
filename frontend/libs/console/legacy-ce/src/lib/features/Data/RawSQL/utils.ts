import { hasuraToast } from '@hasura/shared/ui';
import type { NativeDriver } from '@hasura/shared/types';

// NOTE: Raw SQL is disabled for Cockroach due to this https://github.com/hasura/graphql-engine/issues/8794
export const unsupportedRawSQLDrivers = ['cockroach'];

export const getSourceDriver = (
  dataSources: { name: string; kind?: string }[],
  source: string,
): NativeDriver => {
  const sourceObject = dataSources.find(({ name }) => name === source);
  return (sourceObject?.kind as NativeDriver) || 'postgres';
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
