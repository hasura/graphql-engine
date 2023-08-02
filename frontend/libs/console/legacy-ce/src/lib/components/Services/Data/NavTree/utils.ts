import { Table } from '../../../../features/hasura-metadata-types';

export const adaptTableObject = (table: Table): string[] => {
  if (Array.isArray(table)) return table;

  // This is a safe assumption to make because the only native database that supports functions is postgres( and variants)
  if (typeof table === 'string') return ['public', table];

  const postgresOrMssqlTable = table as {
    schema: string;
    name: string;
  };

  if ('schema' in postgresOrMssqlTable)
    return [postgresOrMssqlTable.schema, postgresOrMssqlTable.name];

  const bigQueryTable = table as { dataset: string; name: string };

  if ('dataset' in bigQueryTable)
    return [bigQueryTable.dataset, bigQueryTable.name];

  return [];
};

export function convertToTreeDataForGDCSource(
  tables: string[][],
  key: string[],
  dataSourceName: string,
  tableLevelClick: (t: Table) => void
): any {
  console.log(tables);
  if (tables.length === 0) return [];

  if (tables[0].length === 1) {
    const leafNodes: any = tables.map(table => {
      return {
        id: JSON.stringify({
          database: dataSourceName,
          table: [...key, table[0]],
        }),
        title: table[0],
        onTableSelect: tableLevelClick,
        isTable: true,
      };
    });

    return leafNodes;
  }

  const uniqueLevelValues = Array.from(new Set(tables.map(table => table[0])));

  const acc: any = [];

  const values = uniqueLevelValues.reduce((_acc, levelValue) => {
    // eslint-disable-next-line no-underscore-dangle
    const _childTables = tables
      .filter(table => table[0] === levelValue)
      .map<string[]>(table => {
        console.log(table);
        return table.slice(1);
      });

    return [
      ..._acc,
      {
        id: JSON.stringify([...key, levelValue[0]]),
        title: levelValue,
        children: convertToTreeDataForGDCSource(
          _childTables,
          [...key, levelValue],
          dataSourceName,
          tableLevelClick
        ),
      },
    ];
  }, acc);

  return values;
}
