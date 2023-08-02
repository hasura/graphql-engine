import { Table } from '../../../hasura-metadata-types';

export const getTableName = (
  table: Table,
  databaseHierarchy: string[]
): string => {
  if (table === null || table === undefined)
    throw Error('Table cannot be null or undefined');

  if (typeof table === 'string') return table;

  if (Array.isArray(table)) {
    // verify if every entry in the array is a string
    const result = table.map<string>(item => {
      if (typeof item === 'string') return item;
      throw Error('Non string values found in table');
    });

    return result.join('.');
  }

  if (typeof table === 'object') {
    if (!databaseHierarchy || !databaseHierarchy.length)
      throw Error('No database hierarchy found');

    const flatJsonTableDefinition = Object.entries(table).reduce<
      Record<string, string>
    >((acc, item) => {
      const [key, value] = item;
      if (typeof key === 'string' && typeof value === 'string')
        acc[key] = value;
      return acc;
    }, {});

    const tableName = databaseHierarchy
      .map(key => {
        if (flatJsonTableDefinition[key]) return flatJsonTableDefinition[key];
        throw Error('unable to find hierachy value');
      })
      .join('.');
    return tableName;
  }

  throw Error('Table name could be generated for the given table type');
};
