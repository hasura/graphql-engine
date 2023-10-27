import {
  getFunctionTreeId,
  getSourceTreeId,
  getTableTreeId,
} from './NavTree/selectors';

export function getTreeIdFromUrl() {
  const params = new URLSearchParams(document.location.search);
  const database = params.get('database');
  const table = params.get('table');
  const func = params.get('function');

  if (document.location.pathname === '/data/v2/manage/database' && database) {
    return getSourceTreeId(database);
  }

  // this catches table/browse, table/modify, table/permissions, table/relationships
  if (
    document.location.pathname.includes('/data/v2/manage/table/') &&
    table &&
    database
  ) {
    return getTableTreeId({
      dataSourceName: database,
      table: JSON.parse(table),
    });
  }

  if (
    document.location.pathname.includes('/data/v2/manage/function/') &&
    database &&
    func
  ) {
    return getFunctionTreeId({
      dataSourceName: database,
      func: JSON.parse(func),
    });
  }

  return undefined;
}
