import { Table } from '@/features/MetadataAPI';
import React from 'react';
import { DataGrid } from '../DataGrid/DataGrid';

interface BrowseRowsContainerProps {
  table: Table;
  dataSourceName: string;
}

export const BrowseRowsContainer = (props: BrowseRowsContainerProps) => {
  return <DataGrid {...props} />;
};
