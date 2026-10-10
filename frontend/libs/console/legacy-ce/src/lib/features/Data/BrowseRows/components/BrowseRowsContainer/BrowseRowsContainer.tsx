import { Source, Table } from '@hasura/shared/types';
import { BrowseRows } from '../../BrowseRows';
import { useTableColumns } from '@hasura/metadata/data-source';
import { useInitialWhereAndOrderBy } from './hooks/useInitialWhereAndOrderBy';

interface BrowseRowsContainerProps {
  table: Table;
  source: Source;
}

export const BrowseRowsContainer = ({
  table,
  source,
}: BrowseRowsContainerProps) => {
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
