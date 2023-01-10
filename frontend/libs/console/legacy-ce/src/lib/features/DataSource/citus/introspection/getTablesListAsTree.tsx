import React from 'react';
import { FaDatabase } from 'react-icons/fa';
import { CitusTable } from '..';
import { exportMetadata } from '../../api';
import { convertToTreeData } from '../../common/utils';
import { NetworkArgs } from '../../types';

export const getTablesListAsTree = async ({
  dataSourceName,
  httpClient,
}: {
  dataSourceName: string;
} & NetworkArgs) => {
  const hierarchy = ['schema', 'name'];

  const { metadata } = await exportMetadata({ httpClient });

  if (!metadata) throw Error('Unable to fetch metadata');

  const source = metadata.sources.find(s => s.name === dataSourceName);

  if (!source) throw Error('Unable to fetch metadata source');

  const tables = source.tables.map(table => table.table as CitusTable);

  return {
    title: (
      <div className="inline-block">
        {source.name}
        {/* <span className="items-center ml-sm px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold bg-indigo-100 text-indigo-800">
          Experimental
        </span> */}
      </div>
    ),
    key: JSON.stringify({ database: source.name }),
    icon: <FaDatabase />,
    children: convertToTreeData(
      tables,
      hierarchy,
      JSON.stringify({ database: source.name })
    ),
  };
};
