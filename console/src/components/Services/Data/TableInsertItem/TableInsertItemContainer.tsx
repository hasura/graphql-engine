import React, { useEffect, useState } from 'react';
import { useAppDispatch, useAppSelector } from '@/store';
import { setTable } from '../DataActions';
import { fetchEnumOptions, I_RESET, insertItem } from './InsertActions';
import { ColumnName, RowValues } from './InsertItem.types';
import { TableInsertRowItemProps } from './TableInsertRowItem';
import { TableInsertItems } from './TableInsertItems';

type GetButtonTextArgs = {
  insertedRows: number;
  ongoingRequest: boolean;
};

const getButtonText = ({ insertedRows, ongoingRequest }: GetButtonTextArgs) => {
  if (ongoingRequest) {
    return 'Saving...';
  }

  if (insertedRows > 0) {
    return 'Insert Again';
  }

  return 'Save';
};

type TableInsertItemContainerContainer = {
  params: {
    schema: string;
    source: string;
    table: string;
  };
};

export const TableInsertItemContainer = (
  props: TableInsertItemContainerContainer
) => {
  const { table: tableName } = props.params;

  const dispatch = useAppDispatch();

  const [isMigration, setIsMigration] = useState(false);
  const [insertedRows, setInsertedRows] = useState(0);
  const [values, setValues] = useState<Record<ColumnName, RowValues>>({});

  const onColumnUpdate: TableInsertRowItemProps['onColumnUpdate'] = (
    columnName,
    rowValues
  ) => {
    const newValues = { ...values };
    if (!newValues[columnName]) {
      newValues[columnName] = {
        valueNode: rowValues.valueNode,
        nullNode: rowValues.nullNode,
        defaultNode: rowValues.defaultNode,
        insertRadioNode: rowValues.insertRadioNode,
      };
    }
    newValues[columnName] = rowValues;
    setValues(newValues);
  };

  useEffect(() => {
    dispatch(setTable(tableName));
    dispatch(fetchEnumOptions());

    return () => {
      dispatch({ type: I_RESET });
    };
  }, [tableName]);

  const nextInsert = () =>
    setInsertedRows(prevInsertedRows => prevInsertedRows + 1);

  const toggleMigrationCheckBox = () =>
    setIsMigration(prevIsMigration => !prevIsMigration);

  const insert = useAppSelector(store => store.tables.insert);
  const allSchemas = useAppSelector(store => store.tables.allSchemas);
  const tablesView = useAppSelector(store => store.tables.view);
  const currentSchema = useAppSelector(store => store.tables.currentSchema);
  const currentDataSource = useAppSelector(
    store => store.tables.currentDataSource
  );
  const migrationMode = useAppSelector(store => store.main.migrationMode);
  const readOnlyMode = useAppSelector(store => store.main.readOnlyMode);

  const { count } = tablesView;
  const { ongoingRequest, lastError, lastSuccess, clone, enumOptions } = insert;
  const buttonText = getButtonText({
    insertedRows,
    ongoingRequest,
  });

  const onClickClear = () => {
    const form = document.getElementById('insertForm');
    if (!form) {
      return;
    }

    const inputs = form.getElementsByTagName('input');
    Array.from(inputs).forEach(input => {
      switch (input.type) {
        case 'text':
          input.value = '';
          break;
        case 'radio':
        case 'checkbox':
          break;
        default:
      }
    });
  };

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

    dispatch(insertItem(tableName, inputValues, isMigration)).then(() => {
      nextInsert();
    });
  };

  return (
    <TableInsertItems
      toggleMigrationCheckBox={toggleMigrationCheckBox}
      onColumnUpdate={onColumnUpdate}
      isMigration={isMigration}
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
      onClickClear={onClickClear}
      lastError={lastError}
      lastSuccess={lastSuccess}
      buttonText={buttonText}
    />
  );
};
