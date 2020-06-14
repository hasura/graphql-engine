export const getLocalStorageItem = key => {
  return window.localStorage.getItem(key);
};

export const setLocalStorageItem = (key, value) => {
  window.localStorage.setItem(key, value);
};

/** Local storage keys **/

export const LS_VERSION_UPDATE_CHECK_LAST_CLOSED =
  'versionUpdateCheck: lastClosed';

export const LS_RAW_SQL_STATEMENT_TIMEOUT = 'rawSql:rawSqlStatementTimeout';
