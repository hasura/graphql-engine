import { getEntries } from '../../../../components/Services/Data/Common/tsUtils';
import { DataNode } from 'antd/lib/tree';
import React from 'react';
import { FaTable, FaFolder } from 'react-icons/fa';
import { TableColumn } from '../../types';
import { TbMathFunction } from 'react-icons/tb';

export function convertToTreeData(
  tables: string[][],
  key: string[],
  dataSourceName: string,
  mode?: 'function'
): DataNode[] {
  if (tables.length === 0) return [];

  if (tables[0].length === 1) {
    const leafNodes: DataNode[] = tables.map(table => {
      return {
        icon:
          mode === 'function' ? (
            <TbMathFunction className="text-muted mr-xs" />
          ) : (
            <FaTable />
          ),
        key:
          mode === 'function'
            ? JSON.stringify({
                database: dataSourceName,
                function: [...key, table[0]],
              })
            : JSON.stringify({
                database: dataSourceName,
                table: [...key, table[0]],
              }),
        title: table[0],
      };
    });

    return leafNodes;
  }

  const uniqueLevelValues = Array.from(new Set(tables.map(table => table[0])));

  const acc: DataNode[] = [];

  const values = uniqueLevelValues.reduce<DataNode[]>((_acc, levelValue) => {
    // eslint-disable-next-line no-underscore-dangle
    const _childTables = tables
      .filter(table => table[0] === levelValue)
      .map<string[]>(table => table.slice(1));

    return [
      ..._acc,
      {
        icon: <FaFolder />,
        selectable: false,
        key: JSON.stringify([...key, levelValue[0]]),
        title: levelValue,
        children: convertToTreeData(
          _childTables,
          [...key, levelValue],
          dataSourceName
        ),
      },
    ];
  }, acc);

  return values;
}

export function adaptAgentDataType(
  sqlDataType: string
): TableColumn['dataType'] {
  const DataTypeToSQLTypeMap: Record<TableColumn['dataType'], string[]> = {
    bool: ['bool'],
    string: ['string'],
    number: ['number', 'integer', 'float'],
    datetime: ['datetime'],
    timestamp: ['timestamp'],
    xml: ['xml'],
    json: ['json', 'jsonb'],
  };

  const [dataType] = getEntries(DataTypeToSQLTypeMap).find(([, value]) =>
    value.includes(
      // Check if sqlDataType is a string or an object
      // Reason is `Error: sqlDataType.toLowerCase is not a function`
      /*
        sqlDataType ->
          {
            element_type: "string",
            nullable: false,
            type: "array"
          }
      */
      typeof sqlDataType === 'string'
        ? sqlDataType.toLowerCase()
        : (sqlDataType as any).type.toLowerCase() // Doubt: Could also be element_type in the case of arrays
    )
  ) ?? ['string', []];

  return dataType;
}
