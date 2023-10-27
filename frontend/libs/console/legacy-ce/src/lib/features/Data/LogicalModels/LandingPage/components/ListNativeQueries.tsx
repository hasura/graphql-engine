import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React from 'react';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { Button } from '../../../../../new-components/Button';
import { CardedTableFromReactTable } from '../../components/CardedTableFromReactTable';
import { NativeQueryWithSource } from '../../types';

const columnHelper = createColumnHelper<NativeQueryWithSource>();

export const ListNativeQueries = ({
  nativeQueries,
  onEditClick,
  onRemoveClick,
}: {
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
            <Button icon={<FaEdit />} onClick={() => onEditClick(row.original)}>
              Edit
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

  return (
    <CardedTableFromReactTable
      table={table}
      noRowsMessage="No Native Queries found."
    />
  );
};
