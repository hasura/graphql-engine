import { OrderBy, WhereClause } from '@hasura/shared/types';

export type FilterFormValues = {
  column: string;
  operator: string;
  /** text, or e.g. a list for `_in`, a boolean for `_is_null` or a date */
  value: unknown;
};

export type SortFormValues = {
  column: string;
  type: 'asc' | 'desc' | '--';
};

export type FiltersAndSortFormValues = {
  filters: FilterFormValues[];
  sorts: SortFormValues[];
};

export const defaultColumn = '-- column --';
export const defaultOperator = '_eq';
export const defaultOrder = '--';

export type UserQuery = {
  where: Record<'_and', WhereClause[]>;
  order_by: OrderBy[];
};
