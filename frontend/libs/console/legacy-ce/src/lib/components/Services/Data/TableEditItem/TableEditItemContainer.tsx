import React, { useEffect, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../../../storeHooks';
import { findTable, generateTableDef } from '../../../../dataSources';
import { setTable } from '../DataActions';
import { fetchEnumOptions, editItem, E_ONGOING_REQ } from './EditActions';
import { ColumnName } from '../TableCommon/DataTableRowItem.types';
import { TableEditItems } from './TableEditItems';
import { ordinalColSort } from '../utils';
import { useSchemas } from '../TableInsertItem/hooks/useSchemas';
import { isColumnAutoIncrement } from '../Common/Components/utils';

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
  const {
    table: tableName,
    source: dataSourceName,
    schema: schemaName,
  } = props.params;

  const dispatch = useAppDispatch();
  const update = useAppSelector(store => store.tables.update);
  const { ongoingRequest, lastError, lastSuccess, enumOptions, oldItem } =
    update;

  const [touchedValues, setTouchedValues] = useState<Record<string, boolean>>(
    {}
  );

  const [values, setValues] = useState<Record<ColumnName, unknown>>(
    oldItem ?? {}
  );
  const onColumnUpdate = (columnName: string, value: unknown) => {
    setValues({ ...values, [columnName]: value });
    setTouchedValues(prev => ({
      ...prev,
      [columnName]: true,
    }));
  };

  useEffect(() => {
    dispatch(setTable(tableName));
    dispatch(fetchEnumOptions());
  }, [tableName]);

  const allSchemas = useAppSelector(store => store.tables.allSchemas);
  const tablesView = useAppSelector(store => store.tables.view);
  const currentSchema = useAppSelector(store => store.tables.currentSchema);
  const currentDataSource = useAppSelector(
    store => store.tables.currentDataSource
  );
  const migrationMode = useAppSelector(store => store.main.migrationMode);
  const readOnlyMode = useAppSelector(store => store.main.readOnlyMode);

  const { count } = tablesView;

  const buttonText = getButtonText({
    ongoingRequest,
  });

  const { data: schemas, isLoading: schemasIsLoading } = useSchemas({
    dataSourceName,
    schemaName,
  });

  const [nullCheckedValues, setNullCheckedValues] = useState<
    Record<string, boolean>
  >({});

  const handleNullChecks = (columnName: string, value: boolean) => {
    setNullCheckedValues({
      ...nullCheckedValues,
      [columnName]: value,
    });
    setTouchedValues(prev => ({
      ...prev,
      [columnName]: true,
    }));
  };

  const [defaultValueColumns, setDefaultValueColumns] = useState<
    Record<string, boolean>
  >({});

  const handleDefaultValueColumns = (columnName: string, value: boolean) => {
    setDefaultValueColumns({
      ...defaultValueColumns,
      [columnName]: value,
    });
    setTouchedValues(prev => ({
      ...prev,
      [columnName]: true,
    }));
  };

  const onClickSave: React.MouseEventHandler = e => {
    e.preventDefault();

    const currentTable = findTable(
      schemas,
      generateTableDef(tableName, schemaName)
    );

    const columns = currentTable?.columns.sort(ordinalColSort) || [];
    const inputValues = columns.reduce((tally, col) => {
      const colName = col.column_name;
      if (!touchedValues[colName]) {
        return tally;
      }

      if (defaultValueColumns[colName]) {
        return {
          ...tally,
          [colName]: { default: true },
        };
      }
      if (nullCheckedValues[colName]) {
        return {
          ...tally,
          [colName]: null,
        };
      }

      const isAutoIncrement = isColumnAutoIncrement(col);
      if (!isAutoIncrement && typeof values[colName] !== 'undefined') {
        return { ...tally, [colName]: values[colName] };
      }
      return tally;
    }, {});

    dispatch({ type: E_ONGOING_REQ });
    dispatch(editItem(tableName, inputValues));
  };

  if (schemasIsLoading) return <p>Loading...</p>;

  return (
    <TableEditItems
      values={values}
      setNullCheckedValues={handleNullChecks}
      setDefaultValueColumns={handleDefaultValueColumns}
      oldItem={oldItem}
      onColumnUpdate={onColumnUpdate}
      dispatch={dispatch}
      tableName={tableName}
      currentSchema={currentSchema}
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
