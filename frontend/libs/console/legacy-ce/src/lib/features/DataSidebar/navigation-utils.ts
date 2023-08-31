import { sessionStore } from '../../utils/sessionStorage';
import { DEFAULT_MANAGE_TABLE_TAB } from './constants';

export const manageTableUrl = ({
  table,
  dataSourceName,
}: {
  dataSourceName: string;
  table: unknown;
}) => {
  const tab =
    sessionStore.getItem('manageTable.lastTab') ?? DEFAULT_MANAGE_TABLE_TAB;

  return `data/v2/manage/table/${tab}?database=${dataSourceName}&table=${encodeURIComponent(
    JSON.stringify(table)
  )}`;
};

export const manageDatabaseUrl = (dataSourceName: string) =>
  `/data/v2/manage/database?database=${encodeURIComponent(dataSourceName)}`;

export const manageFunctionUrl = ({
  fn,
  dataSourceName,
}: {
  dataSourceName: string;
  fn: unknown;
}) =>
  `data/v2/manage/function?database=${dataSourceName}&function=${encodeURIComponent(
    JSON.stringify(fn)
  )}`;
