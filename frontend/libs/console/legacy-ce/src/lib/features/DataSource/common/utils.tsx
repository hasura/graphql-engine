import { FaFolder, FaTable } from 'react-icons/fa';
import React from 'react';
import { MetadataTable, Source, Table } from '../../hasura-metadata-types';
import { IntrospectedTable, TableFkRelationships, TableRow } from '../types';
import { RunSQLResponse } from '../api';
import { getQualifiedTable } from '../../Data/ManageTable/utils';

export const adaptIntrospectedTables = (
  runSqlResponse: RunSQLResponse
): IntrospectedTable[] => {
  /* 
    The `slice(1)` on the result is done because the first item of the result is always the columns names from the SQL output.
    It is not required for the final result and should be avoided 
  */
  const adaptedResponse = runSqlResponse?.result
    ?.slice(1)
    .map((row: string[]) => ({
      name: `${row[1]}.${row[0]}`,
      table: {
        name: row[0],
        schema: row[1],
      },
      type: row[2],
    }));

  return adaptedResponse ?? [];
};

export const convertToTreeData = (
  tables: Table[],
  hierarchy: string[],
  name: string
): any => {
  if (!hierarchy.length) return;

  const key = hierarchy[0];

  function onlyUnique(value: any, index: any, self: string | any[]) {
    return self.indexOf(value) === index;
  }

  const levelValues: string[] = tables.map((t: any) => t[key]).filter(Boolean);

  const uniqueLevelValues = levelValues.filter(onlyUnique);

  return [
    ...uniqueLevelValues.map(levelValue => {
      const { database, ...rest } = JSON.parse(name);
      // eslint-disable-next-line no-underscore-dangle
      const _key = JSON.stringify({
        database,
        table: { ...rest.table, [key]: levelValue },
      });
      const children = convertToTreeData(
        tables.filter((t: any) => t[key] === levelValue),
        hierarchy.slice(1),
        _key
      );

      if (!children)
        return {
          icon: <FaTable />,
          title: levelValue,
          key: _key,
        };

      return {
        icon: <FaFolder />,
        title: levelValue,
        selectable: false,
        children,
        key: _key,
      };
    }),
  ];
};

export const transformGraphqlResponse = ({
  data,
  tableCustomization,
}: {
  data: Record<string, string>[];
  tableCustomization: MetadataTable['configuration'];
  sourceCustomization: Source['customization'];
  columns: string[];
}): TableRow[] => {
  return data.map(row => {
    const transformedRow = Object.entries(row).reduce((acc, [key, value]) => {
      const columnName =
        Object.entries(tableCustomization?.column_config ?? {}).find(
          ([, columnConfig]) => {
            return columnConfig.custom_name === key;
          }
        )?.[0] ?? key;

      return {
        ...acc,
        [columnName]: value,
      };
    }, {});

    return transformedRow;
  });
};

export function generateForeignKeyLabel(foreignKey: TableFkRelationships) {
  const toTableLabel = getQualifiedTable(foreignKey.to.table).join('.');

  return `${foreignKey.from.column
    .join(',')
    // Replace double quotes with empty string
    .replace(/"/g, '')} → ${toTableLabel}.${foreignKey.to.column
    .join(',')
    .replace(/"/g, '')}`;
}
