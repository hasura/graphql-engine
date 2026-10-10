import { FaDatabase } from 'react-icons/fa';
import { MssqlTable } from '../types';
import { convertToTreeData } from '../../common/utils';
import { exportMetadata } from '@hasura/metadata/api';
import { GetTablesListAsTreeProps } from '../../types';

export const getTablesListAsTree = async ({
  dataSourceName,
  endpoints,
  fetchJson,
}: GetTablesListAsTreeProps) => {
  const hierarchy = ['schema', 'name'];

  const { metadata } = await exportMetadata({
    fetchJson,
    url: endpoints.metadata,
  });

  if (!metadata) throw Error('Unable to fetch metadata');

  const source = metadata.sources.find((s) => s.name === dataSourceName);

  if (!source) throw Error('Unable to fetch metadata source');

  const tables = source.tables.map((table) => table.table as MssqlTable);

  return {
    title: (
      <div className="inline-block">
        {source.name}
        {/* <span className="items-center ml-2 px-sm py-0.5 rounded-full text-sm tracking-wide font-semibold bg-indigo-100 text-indigo-800">
          Experimental
        </span> */}
      </div>
    ),
    key: JSON.stringify({ database: source.name }),
    icon: <FaDatabase />,
    children: convertToTreeData(
      tables,
      hierarchy,
      JSON.stringify({ database: source.name }),
    ),
  };
};
