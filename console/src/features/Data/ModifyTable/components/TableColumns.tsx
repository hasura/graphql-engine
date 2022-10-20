import { useTableColumns } from '@/features/BrowseRows';
import React from 'react';
import { ManageTableProps } from '../../ManageTable';
import { TableColumnDescription } from './TableColumnDescription';

export const TableColumns: React.VFC<ManageTableProps> = props => {
  const columns = useTableColumns(props);
  return (
    <>
      {columns.map(c => (
        <TableColumnDescription column={c} key={c.name} />
      ))}
    </>
  );
};
