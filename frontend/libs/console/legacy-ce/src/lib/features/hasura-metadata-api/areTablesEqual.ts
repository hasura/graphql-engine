import { Table } from '@/features/hasura-metadata-types';
import isEqual from 'lodash.isequal';

// giving this it's own module so it doesn't get mixed up with utils

export const areTablesEqual = (table1: Table, table2: Table) => {
  const values1 = Object.values(table1 as any).sort();
  const values2 = Object.values(table2 as any).sort();

  return isEqual(values1, values2);
};
