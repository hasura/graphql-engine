import { isArray, isObject } from '../../components/Common/utils/jsUtils';
import { Table } from '../hasura-metadata-types';
import isEqual from 'lodash/isEqual';

const isObjectOrArray = (table: Table) => isObject(table) || isArray(table);

export const areTablesEqual = (table1: Table, table2: Table) => {
  const values1 = isObjectOrArray(table1)
    ? Object.values(table1 as any).sort()
    : table1;

  const values2 = isObjectOrArray(table2)
    ? Object.values(table2 as any).sort()
    : table2;

  return isEqual(values1, values2);
};
