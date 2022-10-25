import { IndicatorCard } from '@/new-components/IndicatorCard';
import React, { useState } from 'react';
import Skeleton from 'react-loading-skeleton';
import { ManageTableProps } from '../../ManageTable';
import { useListAllTableColumns } from '../hooks/useListAllTableColumns';
import { ModifyTableColumn } from '../types';
import { EditTableColumnDialog } from './EditTableColumnDialog/EditTableColumnDialog';
import { TableColumnDescription } from './TableColumnDescription';

export const TableColumns: React.VFC<ManageTableProps> = props => {
  const { data: columns, isLoading, isError } = useListAllTableColumns(props);

  const [isEditColumnFormActive, setIsEditColumnFormActive] = useState(false);
  const [selectedColumn, setSelectedColumn] = useState<ModifyTableColumn>();

  const resetDialogState = () => {
    setSelectedColumn(undefined);
    setIsEditColumnFormActive(false);
  };

  if (isLoading || !columns) return <Skeleton count={5} height={20} />;

  if (isError)
    return (
      <IndicatorCard status="negative" headline="error">
        Unable to fetch columns
      </IndicatorCard>
    );

  return (
    <>
      {(columns ?? []).map(c => (
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
