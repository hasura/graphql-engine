import type { TreeDataNode } from '@hasura/shared/ui';
import { FaTable, FaFolder } from 'react-icons/fa';
import { TableColumn } from '../../types';
import { TbMathFunction } from 'react-icons/tb';

export function convertToTreeData(
  tables: string[][],
  key: string[],
  dataSourceName: string,
  mode?: 'function',
): TreeDataNode[] {
  if (tables.length === 0) return [];

  if (tables[0].length === 1) {
    const leafNodes: TreeDataNode[] = tables.map((table) => {
      return {
        icon:
          mode === 'function' ? (
            <TbMathFunction className="text-muted mr-1" />
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

  const uniqueLevelValues = Array.from(
    new Set(tables.map((table) => table[0])),
  );

  const acc: TreeDataNode[] = [];

  const values = uniqueLevelValues.reduce<TreeDataNode[]>(
    (_acc, levelValue) => {
      const _childTables = tables
        .filter((table) => table[0] === levelValue)
        .map<string[]>((table) => table.slice(1));

      return [
        ..._acc,
        {
          icon: <FaFolder />,
          selectable: false,
          key: JSON.stringify([...key, levelValue]),
          title: levelValue,
          children: convertToTreeData(
            _childTables,
            [...key, levelValue],
            dataSourceName,
          ),
        },
      ];
    },
    acc,
  );

  return values;
}

export function adaptAgentDataType(
  sqlDataType: TableColumn['dataType'],
): TableColumn['dataType'] {
  return typeof sqlDataType === 'string'
    ? sqlDataType.toLowerCase()
    : sqlDataType.type.toLowerCase();
}
