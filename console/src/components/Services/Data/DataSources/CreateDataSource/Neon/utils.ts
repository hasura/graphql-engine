import { LS_KEYS } from '@/utils/localStorage';

export const NEON_CALLBACK_SEARCH = LS_KEYS.neonCallbackSearch;

export const clearPersistedNeonCallbackSearch = () => {
  window.localStorage.removeItem(NEON_CALLBACK_SEARCH);
};

export const persistNeonCallbackSearch = (value: string) => {
  window.localStorage.setItem(NEON_CALLBACK_SEARCH, value);
};

export const getPersistedNeonCallbackSearch = () => {
  return window.localStorage.getItem(NEON_CALLBACK_SEARCH);
};

export function getNeonDBName(allDatabases: string[]) {
  if (!allDatabases.includes('default')) {
    return 'default';
  }

  const prefix = 'neon-db';
  let suffix = 0;
  let dbName = prefix;
  while (allDatabases.includes(dbName)) {
    dbName = `${prefix}-${++suffix}`;
  }

  return dbName;
}
