import { Table } from '../../hasura-metadata-types';

export const getQualifiedTable = (table: Table): string[] => {
  if (Array.isArray(table)) return table;

  // This is a safe assumption to make because the only native database that supports functions is postgres( and variants)
  if (typeof table === 'string') return ['public', table];

  const postgresOrMssqlTable = table as {
    schema: string;
    name: string;
  };

  if ('schema' in postgresOrMssqlTable)
    return [postgresOrMssqlTable.schema, postgresOrMssqlTable.name];

  const bigQueryTable = table as { dataset: string; name: string };

  if ('dataset' in bigQueryTable)
    return [bigQueryTable.dataset, bigQueryTable.name];

  return [];
};

type PaginateProps<TData> = {
  data: TData[];
  pageNumber: number;
  pageSize: number;
};

type PaginationResult<T> = {
  data: T[];
  totalPages: number;
};

export const paginate = <T>({
  pageNumber,
  pageSize,
  data,
}: PaginateProps<T>): PaginationResult<T> => {
  const startIndex = (pageNumber - 1) * pageSize;
  const endIndex = startIndex + pageSize;
  const paginatedData = data.slice(startIndex, endIndex);
  const totalPages = Math.ceil(data.length / pageSize);
  return { data: paginatedData, totalPages };
};

export function search<T>({
  data,
  searchText,
  searchFn,
}: {
  data: T[];
  searchText: string;
  searchFn: (searchText: string, item: T) => boolean;
}) {
  if (!searchText.length) return data;

  return data.filter(item => searchFn(searchText, item));
}
