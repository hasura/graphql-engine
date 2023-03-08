import { Table } from '../../../hasura-metadata-types';
import React from 'react';
import { BrowseRows } from '../../BrowseRows';
import { useTableColumns } from '../../hooks';
import { useInitialWhereAndOrderBy } from './hooks/useInitialWhereAndOrderBy';

interface BrowseRowsContainerProps {
  table: Table;
  dataSourceName: string;
}

export const BrowseRowsContainer = ({
  table,
  dataSourceName,
}: BrowseRowsContainerProps) => {
  const { data: tableColumns } = useTableColumns({
    table,
    dataSourceName,
  });

  const primaryKeys = (tableColumns?.columns || [])
    .filter(column => column.isPrimaryKey)
    .map(column => column.graphQLProperties?.name)
    .filter(columnName => columnName !== undefined) as string[];

  const { options, onUpdateOptions } = useInitialWhereAndOrderBy({
    columns: tableColumns?.columns,
    table,
    dataSourceName,
  });

  return (
    <div className="p-2">
      <BrowseRows
        table={table}
        dataSourceName={dataSourceName}
        primaryKeys={primaryKeys}
        options={options}
        onUpdateOptions={onUpdateOptions}
      />
    </div>
  );
};
