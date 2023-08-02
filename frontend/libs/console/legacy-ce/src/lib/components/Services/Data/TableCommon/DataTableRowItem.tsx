import React from 'react';
import { TableColumn } from '../../../../dataSources';
import { TableRow } from '../Common/Components/TableRow';

export type DataTableRowItemProps = {
  column: TableColumn;
  onColumnUpdate: (columnName: string, rowValues: unknown) => void;
  enumOptions: any;
  index: string;
  defaultValue?: any;
  values: Record<string, unknown>;
  setNullCheckedValues: (colName: string, isNullChecked: boolean) => void;
  setDefaultValueColumns: (colName: string, isDefaultChecked: boolean) => void;
};

export const DataTableRowItem = ({
  column,
  onColumnUpdate,
  enumOptions,
  index,
  defaultValue = undefined,
  values,
  setNullCheckedValues,
  setDefaultValueColumns,
}: DataTableRowItemProps) => {
  const { column_name: columnName } = column;

  const onChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    onColumnUpdate(columnName, e.target.value);
  };

  return (
    <TableRow
      setNullCheckedValues={setNullCheckedValues}
      setDefaultValueColumns={setDefaultValueColumns}
      values={values}
      column={column}
      prevValue={defaultValue}
      enumOptions={enumOptions}
      onChange={onChange}
      index={index}
    />
  );
};
