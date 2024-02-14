import Skeleton from 'react-loading-skeleton';
import {
  MetadataSelectors,
  useMetadata,
} from '../../../../hasura-metadata-api';
import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React from 'react';
import { useCardedTableFromReactTableWithRef } from '../../components/CardedTableFromReactTable';
import { Button } from '../../../../../new-components/Button';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { NativeQueryRelationship } from '../../../../hasura-metadata-types';

export type ListNativeQueryRow = NativeQueryRelationship & {
  type: 'object' | 'array';
};

export type ListNativeQueryRelationships = {
  dataSourceName: string;
  nativeQueryName: string;
  onDeleteRow?: (data: ListNativeQueryRow) => void;
  onEditRow?: (data: ListNativeQueryRow) => void;
};

const columnHelper = createColumnHelper<ListNativeQueryRow>();

export const ListNativeQueryRelationships = (
  props: ListNativeQueryRelationships
) => {
  const { dataSourceName, nativeQueryName, onDeleteRow, onEditRow } = props;

  const { data: nativeQueryRelationships = [], isLoading } = useMetadata<
    ListNativeQueryRow[]
  >(m => {
    const currentNativeQuery = MetadataSelectors.findNativeQuery(
      dataSourceName,
      nativeQueryName
    )(m);

    return [
      ...(currentNativeQuery?.array_relationships?.map(relationship => ({
        ...relationship,
        type: 'array' as ListNativeQueryRow['type'],
      })) ?? []),
      ...(currentNativeQuery?.object_relationships?.map(relationship => ({
        ...relationship,
        type: 'object' as ListNativeQueryRow['type'],
      })) ?? []),
    ];
  });

  const tableRef = React.useRef<HTMLDivElement>(null);

  const columns = React.useMemo(
    () => [
      columnHelper.accessor('name', {
        id: 'name',
        cell: data => <span>{data.getValue()}</span>,
        header: 'Name',
      }),
      columnHelper.accessor('type', {
        id: 'type',
        cell: data => <span>{data.getValue()}</span>,
        header: 'Type',
      }),
      columnHelper.display({
        id: 'actions',
        cell: ({ row }) => (
          <div className="flex gap-8">
            <Button
              icon={<FaEdit />}
              onClick={() => {
                onEditRow?.(row.original);
              }}
              data-testid="edit-button"
            >
              Edit
            </Button>
            <Button
              icon={<FaTrash />}
              mode="destructive"
              onClick={() => {
                onDeleteRow?.(row.original);
              }}
              data-testid="delete-button"
            >
              Delete
            </Button>
          </div>
        ),
        header: 'Actions',
      }),
    ],
    [onDeleteRow, onEditRow]
  );

  const relationshipsTable = useReactTable({
    data: nativeQueryRelationships,
    columns: columns,
    getCoreRowModel: getCoreRowModel(),
  });

  const NativeQueryRelationshipsTable =
    useCardedTableFromReactTableWithRef<ListNativeQueryRow>();

  if (isLoading) return <Skeleton count={10} height={20} />;

  return (
    <NativeQueryRelationshipsTable
      table={relationshipsTable}
      ref={tableRef}
      noRowsMessage={'No relationships added'}
      dataTestId="native-query-relationships"
    />
  );
};
