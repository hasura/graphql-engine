import { Table } from '@/features/hasura-metadata-types';
import React from 'react';
import { BrowseRows } from '../../BrowseRows';

interface BrowseRowsContainerProps {
  table: Table;
  dataSourceName: string;
}

export const BrowseRowsContainer = (props: BrowseRowsContainerProps) => {
  return (
    <div className="p-2">
      <BrowseRows {...props} />
    </div>
  );
};
