import { isSchemaTable } from '../../DataSource/utils';
import { Table } from '../../hasura-metadata-types';
import isObject from 'lodash/isObject';

/*
this function isn't entirely generic but it will hold for the current set of native DBs we have & GDC as well
*/
export const getTableDisplayName = (table: Table): string => {
  if (Array.isArray(table)) {
    return table.join('.');
  }

  if (!table) {
    return 'Empty Object';
  }

  if (typeof table === 'string') {
    return table;
  }

  if (typeof table === 'object' && isSchemaTable(table)) {
    return table.name;
  }

  if (isObject(table)) {
    const tableObj = table as Record<string, any>;
    return Object.keys(tableObj)
      .sort()
      .map(key => tableObj[key])
      .join('.');
  }

  return JSON.stringify(table);
};
