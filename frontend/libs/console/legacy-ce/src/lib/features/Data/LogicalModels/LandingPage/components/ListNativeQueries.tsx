import { createColumnHelper, useTable } from '@tanstack/react-table';
import React from 'react';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import {
  Button,
  coreTableFeatures,
  CoreTableFeatures,
  CardedTableFromReactTable,
} from '@hasura/shared/ui';
import { NativeQueryWithSource } from '@hasura/metadata/helpers';

const columnHelper = createColumnHelper<
  CoreTableFeatures,
  NativeQueryWithSource
>();

export const ListNativeQueries = ({
  nativeQueries,
  onEditClick,
  onRemoveClick,
}: {
  nativeQueries: NativeQueryWithSource[];
  onEditClick: (model: NativeQueryWithSource) => void;
  onRemoveClick: (model: NativeQueryWithSource) => void;
}) => {
  const columns = React.useMemo(
    () =>
      columnHelper.columns([
        columnHelper.accessor('root_field_name', {
          id: 'name',
          cell: (info) => <span>{info.getValue()}</span>,
          header: (info) => <span>Name</span>,
        }),
        columnHelper.accessor('source', {
          id: 'database',
          cell: (info) => <span>{info.getValue().name}</span>,
          header: 'Database',
        }),
        columnHelper.accessor('returns', {
          id: 'logical_model',
          cell: (info) => <span>{info.getValue()}</span>,
          header: (info) => <span>Logical Model</span>,
        }),
        columnHelper.display({
          id: 'actions',
          header: 'Actions',
          cell: ({ cell, row }) => (
            <Flex gap="2">
              <Button
                size="1"
                leftIcon={FaEdit}
                onClick={() => onEditClick(row.original)}
              >
                Edit
              </Button>
              <Button
                size="1"
                mode="destructive"
                leftIcon={FaTrash}
                onClick={() => onRemoveClick(row.original)}
              >
                Remove
              </Button>
            </Flex>
          ),
        }),
      ]),
    [onEditClick, onRemoveClick],
  );

  const table = useTable({
    features: coreTableFeatures,
    data: nativeQueries ?? [],
    columns,
  });

  return (
    <CardedTableFromReactTable
      table={table}
      noRowsMessage="No Native Queries found."
    />
  );
};
