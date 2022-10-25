import { Table } from '@/features/MetadataAPI';
import React from 'react';
import { DataGrid } from '../DataGrid/DataGrid';

interface BrowseRowsContainerProps {
  table: Table;
  dataSourceName: string;
}

export const BrowseRowsContainer = (props: BrowseRowsContainerProps) => {
  return (
    <div className="p-2">
      <DataGrid {...props} />
    </div>
  );
};
