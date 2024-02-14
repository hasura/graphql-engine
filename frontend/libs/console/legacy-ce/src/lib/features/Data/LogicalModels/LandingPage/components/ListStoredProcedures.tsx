import {
  createColumnHelper,
  getCoreRowModel,
  useReactTable,
} from '@tanstack/react-table';

import { useCallback, useState } from 'react';
import { Button } from '../../../../../new-components/Button';
import { StoredProcedure } from '../../../../hasura-metadata-types';
import { CardedTableFromReactTable } from '../../components/CardedTableFromReactTable';

import { hasuraToast } from '../../../../../new-components/Toasts';
import { useMetadata } from '../../../../hasura-metadata-api';
import { DisplayToastErrorMessage } from '../../../components/DisplayErrorMessage';
import { useTrackStoredProcedure } from '../../../hooks/useTrackStoredProcedure';
import { StoredProcedureDisplayName } from '../../StoredProcedures/components/StoredProcedureDisplayName';
import {
  STORED_PROCEDURE_UNTRACK_ERROR,
  STORED_PROCEDURE_UNTRACK_SUCCESS,
} from '../../constants';
import { useDestructiveAlert } from '../../../../../new-components/Alert';
import { getQualifiedTable } from '../../../ManageTable/utils';

// this is local type for the table row. Do not export
type RowType = { dataSourceName: string } & StoredProcedure;

const columnHelper = createColumnHelper<RowType>();

export const ListStoredProcedures = () => {
  const { untrackStoredProcedure, isLoading } = useTrackStoredProcedure();
  const [activeRow, setActiveRow] = useState<number>();

  /**
   * Get the list of all stored procedures
   */
  const { data = [] } = useMetadata(m =>
    m.metadata.sources
      .map(({ name, stored_procedures }) =>
        (stored_procedures ?? []).map(stored_procedure => ({
          dataSourceName: name,
          ...stored_procedure,
        }))
      )
      .flat()
  );

  // const { hasuraAlert } = useHasuraAlert();

  const { destructiveConfirm } = useDestructiveAlert();

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const onRemoveClick = (data: RowType, index: number) => {
    destructiveConfirm({
      resourceName: getQualifiedTable(data.stored_procedure).join(' / '),
      resourceType: 'Stored Procedure',
      destroyTerm: 'remove',
      onConfirm: () =>
        new Promise(resolve => {
          setActiveRow(index);

          untrackStoredProcedure({
            data: {
              dataSourceName: data.dataSourceName,
              stored_procedure: data.stored_procedure,
            },
            onSuccess: () => {
              hasuraToast({
                type: 'success',
                title: STORED_PROCEDURE_UNTRACK_SUCCESS,
              });
              resolve(true);
            },
            onError: err => {
              hasuraToast({
                type: 'error',
                title: STORED_PROCEDURE_UNTRACK_ERROR,
                children: <DisplayToastErrorMessage message={err.message} />,
              });
              resolve(false);
            },
            onSettled: () => {
              setActiveRow(undefined);
            },
          });
        }),
    });
  };

  const columns = useCallback(
    () => [
      columnHelper.accessor('stored_procedure', {
        id: 'name',
        cell: info => (
          <span>
            <StoredProcedureDisplayName
              qualifiedStoredProcedure={info.getValue()}
            />
          </span>
        ),
        header: info => <span>Name</span>,
      }),
      columnHelper.accessor('dataSourceName', {
        id: 'database',
        cell: info => <span>{info.getValue()}</span>,
        header: info => <span>Database</span>,
      }),
      columnHelper.display({
        id: 'actions',
        header: 'Actions',
        cell: ({ cell, row }) => (
          <div className="flex flex-row gap-2">
            <Button
              mode="destructive"
              onClick={() => onRemoveClick(row.original, row.index)}
              isLoading={activeRow === row.index && isLoading}
            >
              Remove
            </Button>
          </div>
        ),
      }),
    ],
    [activeRow, isLoading, onRemoveClick]
  );

  const table = useReactTable({
    data,
    columns: columns(),
    getCoreRowModel: getCoreRowModel(),
  });

  return (
    <CardedTableFromReactTable
      table={table}
      noRowsMessage="No Tracked Stored Procedures found in metadata."
    />
  );
};
