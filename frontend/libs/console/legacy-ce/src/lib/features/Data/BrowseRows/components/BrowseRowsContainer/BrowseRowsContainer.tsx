import { Source, Table } from '@hasura/shared/types';
import { BrowseRows } from '../../BrowseRows';
import { useTableColumns } from '@hasura/metadata/data-source';
import { useInitialWhereAndOrderBy } from './hooks/useInitialWhereAndOrderBy';

interface BrowseRowsContainerProps {
  table: Table;
  source: Source;
}

/**
 * The tabs, filters and sorting all belong to one table: remount when the
 * route points to another table, so that none of them leak into it.
 */
export const BrowseRowsContainer = ({
  table,
  source,
}: BrowseRowsContainerProps) => (
  <BrowseRowsForTable
    // `table` is parsed from the URL on every render, so compare it by value
    key={`${source.name}:${JSON.stringify(table)}`}
    table={table}
    source={source}
  />
);

const BrowseRowsForTable = ({ table, source }: BrowseRowsContainerProps) => {
  const { data: tableColumns } = useTableColumns({
    table,
    source,
  });

  const { options, onUpdateOptions } = useInitialWhereAndOrderBy({
    columns: tableColumns?.columns,
    table,
    dataSourceName: source.name,
  });

  return (
    <div className="py-4">
      <BrowseRows
        table={table}
        source={source}
        options={options}
        onUpdateOptions={onUpdateOptions}
      />
    </div>
  );
};
