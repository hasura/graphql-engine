import React from 'react';
import { FaDatabase } from 'react-icons/fa';
import { GDCTable } from '..';
import { exportMetadata } from '../../api';
import { NetworkArgs } from '../../types';
import { convertToTreeData } from './utils';

export const getTablesListAsTree = async ({
  dataSourceName,
  httpClient,
}: {
  dataSourceName: string;
} & NetworkArgs) => {
  const { metadata } = await exportMetadata({ httpClient });

  if (!metadata) throw Error('Unable to fetch metadata');

  const source = metadata.sources.find(s => s.name === dataSourceName);

  if (!source) throw Error('Unable to fetch metadata source');

  const tables = source.tables.map(table => {
    if (typeof table.table === 'string') return [table.table] as GDCTable;
    return table.table as GDCTable;
  });

  return {
    title: (
      <div className="inline-block">
        <span className="font-bold text-lg">{source.name}</span>
        <span className="items-center ml-sm px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold bg-indigo-100 text-indigo-800">
          Beta
        </span>
      </div>
    ),
    key: JSON.stringify({ database: source.name }),
    icon: <FaDatabase />,
    children: tables.length ? convertToTreeData(tables, [], source.name) : [],
  };
};
