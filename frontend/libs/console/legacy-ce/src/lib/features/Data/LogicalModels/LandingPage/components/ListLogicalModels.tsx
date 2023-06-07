import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { Button } from '../../../../../new-components/Button';
import { CardedTableFromReactTable } from '../../components/CardedTableFromReactTable';
import { LogicalModelWithSource } from '../../types';

const columnHelper = createColumnHelper<LogicalModelWithSource>();

export const ListLogicalModels = ({
  logicalModels,
  onEditClick,
  onRemoveClick,
  isLoading,
}: {
  isLoading?: boolean;
  logicalModels: LogicalModelWithSource[];
  onEditClick: (model: LogicalModelWithSource) => void;
  onRemoveClick: (model: LogicalModelWithSource) => void;
}) => {
  const columns = React.useCallback(
    () => [
      columnHelper.accessor('name', {
        id: 'name',
        cell: info => <span>{info.getValue()}</span>,
        header: info => <span>Name</span>,
      }),
      columnHelper.accessor('source', {
        id: 'database',
        cell: info => <span>{info.getValue().name}</span>,
        header: info => <span>Database</span>,
      }),
      columnHelper.display({
        id: 'actions',
        header: 'Actions',
        cell: ({ cell, row }) => (
          <div className="flex flex-row gap-2">
            <Button onClick={() => onEditClick(row.original)}>
              Edit Permissions
            </Button>
            <Button
              mode="destructive"
              onClick={() => onRemoveClick(row.original)}
            >
              Remove
            </Button>
          </div>
        ),
      }),
    ],
    [onEditClick, onRemoveClick]
  );

  const table = useReactTable({
    data: logicalModels ?? [],
    columns: columns(),
    getCoreRowModel: getCoreRowModel(),
  });

  if (isLoading) {
    return <Skeleton count={5} height={30} />;
  }

  return (
    <CardedTableFromReactTable
      table={table}
      noRowsMessage="No Logical Models found."
    />
  );
};
