import { OrderBy } from '@hasura/shared/types';
import { ColumnSort } from '@tanstack/react-table';

export const transformToOrderByClause = (sorts: ColumnSort[]): OrderBy[] => {
  return sorts.map((sort) => ({
    column: sort.id,
    type: sort.desc ? 'desc' : 'asc',
  }));
};

// Longest text shown in a cell of the rows table. Very large values (e.g. a
// bytea column holding a file) would otherwise be rendered in full for every
// visible row and freeze the page; the row dialog still shows the full value.
export const MAX_CELL_TEXT_LENGTH = 1000;

export const formatCellValue = (data: unknown) => {
  const text =
    typeof data === 'string' ||
    typeof data === 'number' ||
    typeof data === 'bigint'
      ? String(data)
      : JSON.stringify(data);

  if (text === undefined || text.length <= MAX_CELL_TEXT_LENGTH) {
    return text;
  }

  return `${text.slice(0, MAX_CELL_TEXT_LENGTH)}… (${text.length.toLocaleString()} characters)`;
};
