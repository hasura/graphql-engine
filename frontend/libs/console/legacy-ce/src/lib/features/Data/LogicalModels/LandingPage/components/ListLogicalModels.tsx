import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';
import React, { useMemo } from 'react';
import { FaEdit, FaTrash } from 'react-icons/fa';
import { Badge } from '../../../../../new-components/Badge';
import { Button } from '../../../../../new-components/Button';
import { findReferencedEntities } from '../../LogicalModel/utils/findReferencedEntities';
import { CardedTableFromReactTable } from '../../components/CardedTableFromReactTable';
import { LogicalModelWithSource } from '../../types';

const columnHelper = createColumnHelper<
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
      logicalModels?.map(m => {
        return {
          ...m,
          referencedEntities: findReferencedEntities({
            source: m.source,
            logicalModelName: m.name,
          }),
        };
      }),
    [logicalModels]
  );
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
          <div className="flex-col flex gap-1">
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
          </div>
        ),
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
    data: withReferencesEntities ?? [],
    columns: columns(),
    getCoreRowModel: getCoreRowModel(),
  });

  return (
    <CardedTableFromReactTable
      table={table}
      noRowsMessage="No Logical Models found."
    />
  );
};
