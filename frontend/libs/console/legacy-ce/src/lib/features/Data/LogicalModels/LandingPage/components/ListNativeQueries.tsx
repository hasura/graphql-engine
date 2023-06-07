import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React from 'react';
import { CgDetailsMore } from 'react-icons/cg';
import { FaTrash } from 'react-icons/fa';
import Skeleton from 'react-loading-skeleton';
import { Button } from '../../../../../new-components/Button';
import { CardedTableFromReactTable } from '../../components/CardedTableFromReactTable';
import { NativeQueryWithSource } from '../../types';

const columnHelper = createColumnHelper<NativeQueryWithSource>();

export const ListNativeQueries = ({
  nativeQueries,
  onEditClick,
  onRemoveClick,
  isLoading,
}: {
  isLoading?: boolean;
  nativeQueries: NativeQueryWithSource[];
  onEditClick: (model: NativeQueryWithSource) => void;
  onRemoveClick: (model: NativeQueryWithSource) => void;
}) => {
  const columns = React.useCallback(
    () => [
      columnHelper.accessor('root_field_name', {
        id: 'name',
        cell: info => <span>{info.getValue()}</span>,
        header: info => <span>Name</span>,
      }),
      columnHelper.accessor('source', {
        id: 'database',
        cell: info => <span>{info.getValue().name}</span>,
        header: 'Database',
      }),
      columnHelper.accessor('returns', {
        id: 'logical_model',
        cell: info => <span>{info.getValue()}</span>,
        header: info => <span>Logical Model</span>,
      }),
      columnHelper.display({
        id: 'actions',
        header: 'Actions',
        cell: ({ cell, row }) => (
          <div className="flex flex-row gap-2">
            <Button
              // icon={<FaEdit />}
              icon={<CgDetailsMore />}
              onClick={() => onEditClick(row.original)}
            >
              {/* Edit */}
              {/* Change back to Edit once we support it */}
              View
            </Button>
            <Button
              mode="destructive"
              icon={<FaTrash />}
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
    data: nativeQueries ?? [],
    columns: columns(),
    getCoreRowModel: getCoreRowModel(),
  });

  if (isLoading) {
    return <Skeleton count={5} height={30} />;
  }
  return (
    <CardedTableFromReactTable
      table={table}
      noRowsMessage="No Native Queries found."
    />
  );
};
