import React from 'react';
import { FaTable, FaDatabase, FaFolder } from 'react-icons/fa';
import { DataNode } from 'antd/lib/tree';
import { GDC_TREE_VIEW_DEV } from '@/utils/featureFlags';
import { GDCSource } from './types';

const source_with_three_levels: GDCSource = {
  name: 'gdc_demo_database',
  kind: 'gdc-sample-reference-agent',
  tables: [
    {
      table: {
        name: 'Album',
        schema: 'public',
        anotherSchema: 'foo',
      },
    },
    {
      table: {
        name: 'Artist',
        schema: 'public1',
        anotherSchema: 'foo',
      },
    },
    {
      table: {
        name: 'ArtistView',
        schema: 'public',
        anotherSchema: 'bar',
      },
    },
    {
      table: {
        name: 'Customer',
        schema: 'baz',
        anotherSchema: 'bar',
      },
    },
    {
      table: {
        name: 'Employee',
        schema: 'baz',
        anotherSchema: 'bar',
      },
    },
    {
      table: {
        name: 'Genre',
        schema: 'public',
        anotherSchema: 'foo',
      },
    },
    {
      table: {
        name: 'InvoiceLine',
        schema: 'public',
        anotherSchema: 'foo',
      },
    },
  ],
};

const getSources = async () => [source_with_three_levels];

// This function needs to be moved to the DAL
// eslint-disable-next-line @typescript-eslint/no-unused-vars
const getTableSchema = async (_driver: string) => {
  return {
    table_schema: {
      type: 'object',
      properties: {
        name: {
          type: 'string',
        },
        schema: {
          type: 'string',
        },
      },
    },

    hierarchy: ['schema', 'anotherSchema', 'name'],
  };
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

// eslint-disable-next-line @typescript-eslint/no-unused-vars
export const getTreeData = async <SourceType,>(): Promise<DataNode[]> => {
  const sources = await getSources();

  const tree = sources.map(async source => {
    const tables = source.tables;
    const { hierarchy } = await getTableSchema(source.kind);

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
