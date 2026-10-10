import { OrderBy, WhereClause } from '@hasura/shared/types';

export type FilterFormValues = {
  column: string;
  operator: string;
  value: string;
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
export const defaultOperator = '$eq';
export const defaultOrder = '--';

export type UserQuery = {
  where: Record<'$and', WhereClause[]>;
  order_by: OrderBy[];
};
