import React, { useState } from 'react';
import { ReactQueryStatusUI, useListAllTableColumns } from '../..';
import { TableColumn } from '../../../DataSource';
import { ModifyTableProps } from '../ModifyTable';
import { ModifyTableColumn } from '../types';
import { EditTableColumnDialog } from './EditTableColumnDialog/EditTableColumnDialog';
import { TableColumnDescription } from './TableColumnDescription';

type TableColumnProps = ModifyTableProps;
export const TableColumns: React.VFC<TableColumnProps> = props => {
  const { dataSourceName, table } = props;
  const { columns, combinedStatus, firstError } = useListAllTableColumns(
    dataSourceName,
    table
  );

  const [isEditColumnFormActive, setIsEditColumnFormActive] = useState(false);
  const [selectedColumn, setSelectedColumn] = useState<ModifyTableColumn>();

  const resetDialogState = () => {
    setSelectedColumn(undefined);
    setIsEditColumnFormActive(false);
  };

  // adding a "combinedStatus" as the loading variable before was not taking into account both queries and was not correctly showing a loader
  if (combinedStatus !== 'success')
    return (
      <ReactQueryStatusUI
        error={firstError}
        status={combinedStatus}
        loader="skeleton"
        skeletonProps={{ count: 2, height: 28, width: 400 }}
      />
    );

  return (
    <>
      {(columns ?? []).map((c: TableColumn) => (
        <TableColumnDescription
          column={c}
          key={c.name}
          onEdit={column => {
            setIsEditColumnFormActive(true);
            setSelectedColumn(column);
          }}
        />
      ))}
      {isEditColumnFormActive && selectedColumn && (
        <EditTableColumnDialog
          {...props}
          column={selectedColumn}
          onClose={resetDialogState}
        />
      )}
    </>
  );
};
