import React, { useEffect, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../../../storeHooks';
import { useReadOnlyMode } from '../../../../hooks';
import { useMetadata } from '../../../../features/MetadataAPI';
import { HasuraMetadataV3 } from '../../../../metadata/types';
import Spinner from '../../../Common/Spinner/Spinner';
import isObject from 'lodash/isObject';

import { fetchEnumOptions, insertItem } from './InsertActions';
import { TableInsertItems } from './TableInsertItems';
import { useSchemas } from './hooks/useSchemas';
import { findTable, generateTableDef } from '../../../../dataSources';
import { ordinalColSort } from '../utils';
import { ColumnName } from '../TableCommon/DataTableRowItem.types';
import { isColumnAutoIncrement } from '../Common/Components/utils';
import { setTable } from '../DataActions';

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

const getTableMetadata = (
  source: string,
  table: string,
  metadata: HasuraMetadataV3 | undefined
) => {
  return metadata?.sources
    ?.find((s: { name: string }) => s.name === source)
    ?.tables.filter(
      (t: { table: { name: string } }) => t?.table?.name === table
    )?.[0];
};

type TableInsertItemContainerContainer = {
  params: {
    schema: string;
    source: string;
    table: string;
  };
  router: { location: { state: any } };
};

export const TableInsertItemContainer = (
  props: TableInsertItemContainerContainer
) => {
  const {
    table: tableName,
    source: dataSourceName,
    schema: schemaName,
  } = props.params;
  const currentRow = props.router?.location?.state?.row;

  const dispatch = useAppDispatch();

  useEffect(() => {
    dispatch(setTable(tableName));
    dispatch(fetchEnumOptions());
  }, [tableName]);

  const [isMigration, setIsMigration] = useState(false);
  const [insertedRows, setInsertedRows] = useState(0);
  const [values, setValues] = useState<Record<ColumnName, unknown>>(
    currentRow ?? {}
  );
  const [nullCheckedValues, setNullCheckedValues] = useState<
    Record<string, boolean>
  >({});

  const handleNullChecks = (columnName: string, value: boolean) => {
    setNullCheckedValues({
      ...nullCheckedValues,
      [columnName]: value,
    });
  };

  const [defaultValueColumns, setDefaultValueColumns] = useState<
    Record<string, boolean>
  >({});

  const handleDefaultValueColumns = (columnName: string, value: boolean) => {
    setDefaultValueColumns({
      ...defaultValueColumns,
      [columnName]: value,
    });
  };

  const migrationMode = useAppSelector(store => store.main.migrationMode);

  const { data: metadata } = useMetadata();
  const tableMetadata = getTableMetadata(
    dataSourceName,
    tableName,
    metadata?.metadata
  );

  const { data: readOnlyMode } = useReadOnlyMode();
  const { data: schemas, isLoading: schemasIsLoading } = useSchemas({
    dataSourceName,
    schemaName,
  });

  const insertTable = useAppSelector(store => store.tables.insert);
  const { enumOptions } = insertTable;

  const onColumnUpdate = (columnName: string, value: unknown) => {
    setValues({ ...values, [columnName]: value });
  };

  const toggleMigrationCheckBox = () =>
    setIsMigration(prevIsMigration => !prevIsMigration);

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

  // Refactor in next iteration: --- Insert section start ---
  const insert = useAppSelector(
    (store: { tables: { insert: any } }) => store.tables.insert
  );
  const { ongoingRequest, lastError, lastSuccess } = insert;

  const nextInsert = () =>
    setInsertedRows(prevInsertedRows => prevInsertedRows + 1);

  const buttonText = getButtonText({
    insertedRows,
    ongoingRequest,
  });

  const onClickSave: React.MouseEventHandler = e => {
    e.preventDefault();

    const currentTable = findTable(
      schemas,
      generateTableDef(tableName, schemaName)
    );

    const columns = currentTable?.columns.sort(ordinalColSort) || [];
    const inputValues = columns.reduce((tally, col) => {
      const colName = col.column_name;
      if (defaultValueColumns[colName]) {
        return tally;
      }
      if (nullCheckedValues[colName]) {
        return {
          ...tally,
          [colName]: null,
        };
      }
      const isAutoIncrement = isColumnAutoIncrement(col);
      if (!isAutoIncrement && typeof values[colName] !== 'undefined') {
        return {
          ...tally,
          [colName]: isObject(values[colName])
            ? JSON.stringify(values[colName])
            : values[colName],
        };
      }
      return tally;
    }, {});

    dispatch(insertItem(tableName, inputValues, isMigration)).then(() => {
      nextInsert();
    });
  };

  if (schemasIsLoading)
    return (
      <p className="m-4">
        <Spinner width="16px" height="16px" />
      </p>
    );

  return (
    <TableInsertItems
      values={values}
      setNullCheckedValues={handleNullChecks}
      setDefaultValueColumns={handleDefaultValueColumns}
      isEnum={!!tableMetadata?.is_enum}
      toggleMigrationCheckBox={toggleMigrationCheckBox}
      onColumnUpdate={onColumnUpdate}
      isMigration={isMigration}
      dispatch={dispatch}
      tableName={tableName}
      currentSchema={schemaName}
      schemas={schemas}
      migrationMode={!!migrationMode}
      readOnlyMode={!!readOnlyMode}
      count={0}
      enumOptions={enumOptions}
      currentSource={dataSourceName}
      onClickSave={onClickSave}
      onClickClear={onClickClear}
      lastError={lastError}
      lastSuccess={lastSuccess}
      buttonText={buttonText}
    />
  );
};
