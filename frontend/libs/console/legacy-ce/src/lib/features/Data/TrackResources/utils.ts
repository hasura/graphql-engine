import { TrackableTable } from './types';

export const paginate = <T>(
  array: T[],
  page_size: number,
  page_number: number
): T[] => {
  // human-readable page numbers usually start with 1, so we reduce 1 in the first argument
  return array.slice((page_number - 1) * page_size, page_number * page_size);
};

export const search = (tables: TrackableTable[], searchText: string) => {
  if (!searchText.length) return tables;

  return tables.filter(table =>
    table.name.toLowerCase().includes(searchText.toLowerCase())
  );
};
