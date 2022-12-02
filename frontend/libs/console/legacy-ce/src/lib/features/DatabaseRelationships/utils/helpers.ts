import { Table } from '@/features/hasura-metadata-types';

/*
this function isn't entirely generic but it will hold for the current set of native DBs we have & GDC as well
*/
export const getTableDisplayName = (table: Table): string => {
  if (Array.isArray(table)) return table.join('.');

  if (!table) return 'Empty Object';

  if (typeof table === 'string') return table;

  if (typeof table === 'object' && 'name' in table)
    return (table as { name: string }).name;

  return JSON.stringify(table);
};
