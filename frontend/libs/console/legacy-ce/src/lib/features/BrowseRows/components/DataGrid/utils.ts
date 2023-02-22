import { OrderBy } from '../../../DataSource';
import { ColumnSort } from '@tanstack/react-table';

export const transformToOrderByClause = (sorts: ColumnSort[]): OrderBy[] => {
  return sorts.map(sort => ({
    column: sort.id,
    type: sort.desc ? 'desc' : 'asc',
  }));
};
