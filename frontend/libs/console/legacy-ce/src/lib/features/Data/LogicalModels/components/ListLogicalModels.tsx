import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React from 'react';
import { Button } from '../../../../new-components/Button';
import { useMetadata } from '../../../hasura-metadata-api';
import { LogicalModel } from '../../../hasura-metadata-types';
import Skeleton from 'react-loading-skeleton';
import { CardedTableFromReactTable } from './CardedTableFromReactTable';

const columnHelper = createColumnHelper<LogicalModel>();

export const ListLogicalModels = ({
  dataSourceName,
  onEditClick,
  onRemoveClick,
}: {
  dataSourceName: string;
  onEditClick: (model: LogicalModel) => void;
  onRemoveClick: (model: LogicalModel) => void;
}) => {
  const { data: logicalModels, isLoading } = useMetadata(
    m => m.metadata.sources.find(s => s.name === dataSourceName)?.logical_models
  );

  const columns = React.useCallback(
    () => [
      columnHelper.accessor('name', {
        id: 'name',
        cell: info => <span>{info.getValue()}</span>,
        header: info => <span>Name</span>,
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
    data: logicalModels ?? [],
    columns: columns(),
    getCoreRowModel: getCoreRowModel(),
  });

  if (isLoading) {
    return <Skeleton count={5} height={30} />;
  }

  return <CardedTableFromReactTable table={table} />;
};
