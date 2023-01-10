import { Table } from '@/features/hasura-metadata-types';
import React from 'react';
import { BrowseRows } from '../../BrowseRows';
import { useTableColumns } from '../../hooks';

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

  return (
    <div className="p-2">
      <BrowseRows
        table={table}
        dataSourceName={dataSourceName}
        primaryKeys={primaryKeys}
      />
    </div>
  );
};
