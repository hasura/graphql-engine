import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React from 'react';
import Skeleton from 'react-loading-skeleton';
import { Button } from '../../../../new-components/Button';
import { useMetadata } from '../../../hasura-metadata-api';
import { NativeQuery } from '../../../hasura-metadata-types';
import { CardedTableFromReactTable } from './CardedTableFromReactTable';

const columnHelper = createColumnHelper<NativeQuery>();

export const ListNativeQueries = ({
  dataSourceName,
  onEditClick,
  onRemoveClick,
}: {
  dataSourceName: string;
  onEditClick: (model: NativeQuery) => void;
  onRemoveClick: (model: NativeQuery) => void;
}) => {
  const { data: nativeQueries, isLoading } = useMetadata(
    m => m.metadata.sources.find(s => s.name === dataSourceName)?.native_queries
  );

  const columns = React.useCallback(
    () => [
      columnHelper.accessor('root_field_name', {
        id: 'name',
        cell: info => <span>{info.getValue()}</span>,
        header: info => <span>Name</span>,
      }),
      columnHelper.display({
        id: 'database',
        cell: () => <span>{dataSourceName}</span>,
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
            <Button onClick={() => onEditClick(row.original)}>Edit</Button>
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
    []
  );

  const table = useReactTable({
    data: nativeQueries ?? [],
    columns: columns(),
    getCoreRowModel: getCoreRowModel(),
  });

  if (isLoading) {
    return <Skeleton count={5} height={30} />;
  }
  return <CardedTableFromReactTable table={table} />;
};
