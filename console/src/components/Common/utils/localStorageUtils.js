export const getLocalStorageItem = key => {
  return window.localStorage.getItem(key);
};

export const setLocalStorageItem = (key, value) => {
  window.localStorage.setItem(key, value);
};

export const upsertLocalStorageObject = (key, object) => {
  const objStr = window.localStorage.getItem(key);
  const oldObj = JSON.parse(objStr);
  window.localStorage.setItem(
    key,
    JSON.stringify({ ...(oldObj && { ...oldObj }), ...object })
  );
};

/** Local storage keys **/

export const LS_VERSION_UPDATE_CHECK_LAST_CLOSED =
  'versionUpdateCheck: lastClosed';

export const LS_RAW_SQL_STATEMENT_TIMEOUT = 'rawSql:rawSqlStatementTimeout';
