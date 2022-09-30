export type DataGridRowSortProps = {
  column: string;
  type: 'asc' | 'desc';
  nulls: 'last';
} | null;

export type UseRowsData = {
  rows: Record<string, string | number>[];
  columns: string[];
  orderBy: DataGridRowSortProps;
};
