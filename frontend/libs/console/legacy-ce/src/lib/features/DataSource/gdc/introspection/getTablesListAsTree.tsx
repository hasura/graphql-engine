import { Badge } from '../../../../new-components/Badge';
import React from 'react';
import { FaDatabase } from 'react-icons/fa';
import { GDCTable } from '..';
import { exportMetadata } from '../../api';
import { GetTablesListAsTreeProps } from '../../types';
import { convertToTreeData } from './utils';
// import { QualifiedFunction } from '../../../hasura-metadata-types';

export const getTablesListAsTree = async ({
  dataSourceName,
  httpClient,
  releaseName,
}: GetTablesListAsTreeProps) => {
  const { metadata } = await exportMetadata({ httpClient });

  if (!metadata) throw Error('Unable to fetch metadata');

  const source = metadata.sources.find(s => s.name === dataSourceName);

  if (!source) throw Error('Unable to fetch metadata source');

  const tables = source.tables.map(table => {
    if (typeof table.table === 'string') return [table.table] as GDCTable;
    return table.table as GDCTable;
  });

  const functions = (source?.functions ?? []).map(f => {
    if (typeof f.function === 'string') return [f.function] as GDCTable;
    return f.function as GDCTable;
  });

  return {
    title: (
      <div className="inline-block">
        <span className="font-bold text-lg">{source.name}</span>
        {releaseName !== 'GA' && !!releaseName && (
          <Badge color="indigo" className="ml-sm">
            {releaseName}
          </Badge>
        )}
      </div>
    ),
    key: JSON.stringify({ database: source.name }),
    icon: <FaDatabase size="16px" />,
    children: tables.length
      ? [
          ...convertToTreeData(tables, [], source.name),
          ...convertToTreeData(functions, [], source.name, 'function'),
        ]
      : [],
  };
};
