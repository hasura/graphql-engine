import { useMetadata } from '@hasura/metadata/api';
import { Flex } from '@radix-ui/themes';
import { createColumnHelper, useTable } from '@tanstack/react-table';
import React from 'react';
import {
  Button,
  SkeletonList,
  coreTableFeatures,
  CoreTableFeatures,
  createCardedTableFromReactTableWithRef,
} from '@hasura/shared/ui';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { NativeQueryRelationship } from '@hasura/shared/types';
import { MetadataSelectors } from '@hasura/metadata/helpers';

export type ListNativeQueryRow = NativeQueryRelationship & {
  type: 'object' | 'array';
};

export type ListNativeQueryRelationships = {
  dataSourceName: string;
  nativeQueryName: string;
  onDeleteRow?: (data: ListNativeQueryRow) => void;
  onEditRow?: (data: ListNativeQueryRow) => void;
};

const columnHelper = createColumnHelper<
  CoreTableFeatures,
  ListNativeQueryRow
>();

const NativeQueryRelationshipsTable =
  createCardedTableFromReactTableWithRef<ListNativeQueryRow>();

export const ListNativeQueryRelationships = (
  props: ListNativeQueryRelationships,
) => {
  const { dataSourceName, nativeQueryName, onDeleteRow, onEditRow } = props;

  const { data: nativeQueryRelationships = [], isLoading } = useMetadata<
    ListNativeQueryRow[]
  >((m) => {
    const currentNativeQuery = MetadataSelectors.findNativeQuery(
      dataSourceName,
      nativeQueryName,
    )(m);

    return [
      ...(currentNativeQuery?.array_relationships?.map((relationship) => ({
        ...relationship,
        type: 'array' as ListNativeQueryRow['type'],
      })) ?? []),
      ...(currentNativeQuery?.object_relationships?.map((relationship) => ({
        ...relationship,
        type: 'object' as ListNativeQueryRow['type'],
      })) ?? []),
    ];
  });

  const tableRef = React.useRef<HTMLDivElement>(null);

  const columns = React.useMemo(
    () =>
      columnHelper.columns([
        columnHelper.accessor('name', {
          id: 'name',
          cell: (data) => <span>{data.getValue()}</span>,
          header: 'Name',
        }),
        columnHelper.accessor('type', {
          id: 'type',
          cell: (data) => <span>{data.getValue()}</span>,
          header: 'Type',
        }),
        columnHelper.display({
          id: 'actions',
          cell: ({ row }) => (
            <Flex gap="8">
              <Button
                leftIcon={FaEdit}
                onClick={() => {
                  onEditRow?.(row.original);
                }}
                data-testid="edit-button"
              >
                Edit
              </Button>
              <Button
                leftIcon={FaTrash}
                mode="destructive"
                onClick={() => {
                  onDeleteRow?.(row.original);
                }}
                data-testid="delete-button"
              >
                Delete
              </Button>
            </Flex>
          ),
          header: 'Actions',
        }),
      ]),
    [onDeleteRow, onEditRow],
  );

  const relationshipsTable = useTable({
    features: coreTableFeatures,
    data: nativeQueryRelationships,
    columns: columns,
  });

  if (isLoading) return <SkeletonList count={5} />;

  return (
    <NativeQueryRelationshipsTable
      table={relationshipsTable}
      ref={tableRef}
      noRowsMessage={'No relationships added'}
      dataTestId="native-query-relationships"
    />
  );
};
