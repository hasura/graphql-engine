import { IndicatorCard } from '../../../../new-components/IndicatorCard';
import React, { useState } from 'react';
import Skeleton from 'react-loading-skeleton';
import { useListAllTableColumns } from '../..';
import { TableColumn } from '../../../DataSource';
import { ModifyTableColumn } from '../types';
import { EditTableColumnDialog } from './EditTableColumnDialog/EditTableColumnDialog';
import { TableColumnDescription } from './TableColumnDescription';
import { ModifyTableProps } from '../ModifyTable';

type TableColumnProps = ModifyTableProps;
export const TableColumns: React.VFC<TableColumnProps> = props => {
  const { dataSourceName, table } = props;
  const { columns, isLoading, isError } = useListAllTableColumns(
    dataSourceName,
    table
  );

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
