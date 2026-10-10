import { Badge } from '@hasura/shared/ui';
import { FaDatabase } from 'react-icons/fa';
import { GDCTable } from '@hasura/shared/types';
import { GetTablesListAsTreeProps } from '../../types';
import { convertToTreeData } from './utils';
import { exportMetadata } from '@hasura/metadata/api';

export const getTablesListAsTree = async ({
  dataSourceName,
  releaseName,
  endpoints,
  fetchJson,
}: GetTablesListAsTreeProps) => {
  const { metadata } = await exportMetadata({
    fetchJson,
    url: endpoints.metadata,
  });

  if (!metadata) throw Error('Unable to fetch metadata');

  const source = metadata.sources.find((s) => s.name === dataSourceName);

  if (!source) throw Error('Unable to fetch metadata source');

  const tables = source.tables.map((table) => {
    if (typeof table.table === 'string') return [table.table] as GDCTable;
    return table.table as GDCTable;
  });

  const functions = (source?.functions ?? []).map((f) => {
    if (typeof f.function === 'string') return [f.function] as GDCTable;
    return f.function as GDCTable;
  });

  return {
    title: (
      <div className="inline-block">
        <span className="font-bold text-lg">{source.name}</span>
        {releaseName !== 'GA' && !!releaseName && (
          <Badge color="indigo" className="ml-2">
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
