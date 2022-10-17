import React, { useEffect, useState } from 'react';
import { useAppDispatch, useAppSelector } from '@/store';
import { setTable } from '../DataActions';
import { fetchEnumOptions, editItem, E_ONGOING_REQ } from './EditActions';
import { ColumnName, RowValues } from '../TableCommon/DataTableRowItem.types';
import { DataTableRowItemProps } from '../TableCommon/DataTableRowItem';
import { TableEditItems } from './TableEditItems';

type GetButtonTextArgs = {
  ongoingRequest: boolean;
};

const getButtonText = ({ ongoingRequest }: GetButtonTextArgs) => {
  if (ongoingRequest) {
    return 'Saving...';
  }

  return 'Save';
};

type TableEditItemContainerContainer = {
  params: {
    schema: string;
    source: string;
    table: string;
  };
};

export const TableEditItemContainer = (
  props: TableEditItemContainerContainer
) => {
  const { table: tableName } = props.params;

  const dispatch = useAppDispatch();

  const [values, setValues] = useState<Record<ColumnName, RowValues>>({});

  const onColumnUpdate: DataTableRowItemProps['onColumnUpdate'] = (
    columnName,
    rowValues
  ) => {
    const newValues = { ...values };
    if (!newValues[columnName]) {
      newValues[columnName] = {
        valueNode: rowValues.valueNode,
        nullNode: rowValues.nullNode,
        defaultNode: rowValues.defaultNode,
        radioNode: rowValues.radioNode,
      };
    }
    newValues[columnName] = rowValues;
    setValues(newValues);
  };

  useEffect(() => {
    dispatch(setTable(tableName));
    dispatch(fetchEnumOptions());
  }, [tableName]);

  const update = useAppSelector(store => store.tables.update);
  const allSchemas = useAppSelector(store => store.tables.allSchemas);
  const tablesView = useAppSelector(store => store.tables.view);
  const currentSchema = useAppSelector(store => store.tables.currentSchema);
  const currentDataSource = useAppSelector(
    store => store.tables.currentDataSource
  );
  const migrationMode = useAppSelector(store => store.main.migrationMode);
  const readOnlyMode = useAppSelector(store => store.main.readOnlyMode);

  const { count } = tablesView;
  const {
    ongoingRequest,
    lastError,
    lastSuccess,
    clone,
    enumOptions,
    oldItem,
  } = update;
  const buttonText = getButtonText({
    ongoingRequest,
  });

  const onClickSave: React.MouseEventHandler = e => {
    e.preventDefault();

    const inputValues = Object.keys(values).reduce<
      Record<ColumnName, string | null | undefined>
    >((acc, colName) => {
      if (values?.[colName]?.nullNode?.checked) {
        acc[colName] = null;
        return acc;
      }

      if (values?.[colName]?.defaultNode?.checked) {
        return acc;
      }

      acc[colName] = values?.[colName]?.valueNode?.value?.toString();

      return acc;
    }, {});

    dispatch({ type: E_ONGOING_REQ });

    dispatch(editItem(tableName, inputValues));
  };

  return (
    <TableEditItems
      ongoingRequest={ongoingRequest}
      oldItem={oldItem}
      onColumnUpdate={onColumnUpdate}
      dispatch={dispatch}
      tableName={tableName}
      currentSchema={currentSchema}
      clone={clone}
      schemas={allSchemas}
      migrationMode={migrationMode}
      readOnlyMode={readOnlyMode}
      count={count}
      enumOptions={enumOptions}
      currentSource={currentDataSource}
      onClickSave={onClickSave}
      lastError={lastError}
      lastSuccess={lastSuccess}
      buttonText={buttonText}
    />
  );
};
