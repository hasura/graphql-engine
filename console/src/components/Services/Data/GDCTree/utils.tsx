import React from 'react';
import { FaTable, FaDatabase, FaFolder } from 'react-icons/fa';
import { DataSource, exportMetadata } from '@/features/DataSource';
// eslint-disable-next-line no-restricted-imports
import { httpClient } from '@/features/DataSource/api';
import { DataNode } from 'antd/lib/tree';
import { GDC_TREE_VIEW_DEV } from '@/utils/featureFlags';
import { GDCSource } from './types';

const getSources = async () => {
  const metadata = await exportMetadata();
  const nativeDrivers = await DataSource(httpClient).getNativeDrivers();
  return metadata.sources
    .filter(source => !nativeDrivers.includes(source.kind))
    .map<GDCSource>(source => ({
      name: source.name,
      kind: source.kind,
      tables: source.tables.map(({ table }) => ({ table })),
    }));
};

const nest = (
  tables: GDCSource['tables'],
  hierarchy: string[],
  name: string
): any => {
  if (!hierarchy.length) return;

  const key = hierarchy[0];

  function onlyUnique(value: any, index: any, self: string | any[]) {
    return self.indexOf(value) === index;
  }

  const levelValues: string[] = tables
    .map((t: any) => t.table[key])
    .filter(Boolean);

  const uniqueLevelValues = levelValues.filter(onlyUnique);

  return [
    ...uniqueLevelValues.map(levelValue => {
      // eslint-disable-next-line no-underscore-dangle
      const _key = JSON.stringify({ ...JSON.parse(name), [key]: levelValue });
      const children = nest(
        tables.filter((t: any) => t.table[key] === levelValue),
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

export const getTreeData = async (): Promise<DataNode[]> => {
  const sources = await getSources();

  const tree = sources.map(async source => {
    const tables = source.tables;

    const hierarchy = await DataSource(httpClient).getDatabaseHierarchy({
      dataSourceName: source.name,
    });

    // return a node of the tree
    return {
      title: (
        <div className="inline-block">
          {source.name}
          <span className="items-center ml-sm px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold bg-indigo-100 text-indigo-800">
            Experimental
          </span>
        </div>
      ),
      key: JSON.stringify({ database: source.name }),
      icon: <FaDatabase />,
      children: nest(
        tables,
        hierarchy,
        JSON.stringify({ database: source.name })
      ),
    };
  });

  // feature flag to enable tree view
  if (GDC_TREE_VIEW_DEV === 'enabled') return Promise.all(tree);

  return [];
};
