import { createColumnHelper, useTable } from '@tanstack/react-table';
import React, { useMemo } from 'react';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { Flex } from '@radix-ui/themes';
import {
  Badge,
  Button,
  coreTableFeatures,
  CoreTableFeatures,
  CardedTableFromReactTable,
} from '@hasura/shared/ui';

import { findReferencedEntities } from '../../LogicalModel/utils/findReferencedEntities';
import { LogicalModelWithSource } from '@hasura/metadata/helpers';

const columnHelper = createColumnHelper<
  CoreTableFeatures,
  LogicalModelWithSource & {
    referencedEntities: ReturnType<typeof findReferencedEntities>;
  }
>();

export const ListLogicalModels = ({
  logicalModels,
  onEditClick,
  onRemoveClick,
}: {
  logicalModels: LogicalModelWithSource[];
  onEditClick: (model: LogicalModelWithSource) => void;
  onRemoveClick: (model: LogicalModelWithSource) => void;
}) => {
  const withReferencesEntities = useMemo(
    () =>
      logicalModels?.map((m) => {
        return {
          ...m,
          referencedEntities: findReferencedEntities({
            source: m.source,
            logicalModelName: m.name,
          }),
        };
      }),
    [logicalModels],
  );
  const columns = React.useMemo(
    () =>
      columnHelper.columns([
        columnHelper.accessor('name', {
          id: 'name',
          cell: (info) => <span>{info.getValue()}</span>,
          header: (info) => <span>Name</span>,
        }),
        columnHelper.accessor('source', {
          id: 'database',
          cell: (info) => <span>{info.getValue().name}</span>,
          header: (info) => <span>Database</span>,
        }),
        columnHelper.display({
          id: 'refs',
          header: 'Used By',
          cell: ({
            row: {
              original: {
                referencedEntities: {
                  tables,
                  native_queries,
                  logical_models,
                  stored_procedures,
                },
              },
            },
          }) => (
            <Flex direction="column" gap="1">
              {!!tables.length && (
                <div>
                  <Badge color="red">Tables: {tables.length}</Badge>
                </div>
              )}
              {!!native_queries.length && (
                <div>
                  <Badge color="blue">
                    Native Queries: {native_queries.length}
                  </Badge>
                </div>
              )}
              {!!stored_procedures.length && (
                <div>
                  <Badge color="green">
                    Stored Procedures: {stored_procedures.length}
                  </Badge>
                </div>
              )}
              {!!logical_models.length && (
                <div>
                  <Badge color="yellow">
                    Logical Models: {logical_models.length}
                  </Badge>
                </div>
              )}
            </Flex>
          ),
        }),
        columnHelper.display({
          id: 'actions',
          header: 'Actions',
          cell: ({ cell, row }) => (
            <Flex direction="row" gap="2">
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
    data: withReferencesEntities ?? [],
    columns,
  });

  return (
    <CardedTableFromReactTable
      table={table}
      noRowsMessage="No Logical Models found."
    />
  );
};
