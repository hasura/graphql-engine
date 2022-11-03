import React, { useState } from 'react';
import { useAppDispatch, useAppSelector } from '@/store';
import { useMigrationMode, useReadOnlyMode } from '@/hooks';
import { useMetadata } from '@/features/MetadataAPI';
import { HasuraMetadataV3 } from '@/metadata/types';
import { insertItem } from './InsertActions';
import { ColumnName, RowValues } from '../TableCommon/DataTableRowItem.types';
import { DataTableRowItemProps } from '../TableCommon/DataTableRowItem';
import { TableInsertItems } from './TableInsertItems';
import {
  useTableEnums,
  UseTableEnumsResponseArrayType,
} from './hooks/useTableEnums';
import { useSchemas } from './hooks/useSchemas';
import { TableObject } from './types';

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

const getTableWithEnumRelations = (
  source: string,
  schema: string,
  metadata: HasuraMetadataV3 | undefined
) => {
  return metadata
    ? (metadata?.sources
        ?.find(s => s.name === source)
        ?.tables.filter((t: any) => {
          return t?.is_enum && t?.table?.schema === schema;
        })
        ?.map(t => t?.table) as TableObject[])
    : [];
};

const formatEnumOptions = (
  tableEnums: UseTableEnumsResponseArrayType | undefined
) =>
  tableEnums
    ? tableEnums?.reduce((tally, curr) => {
        return {
          ...tally,
          [curr.from]: curr.values,
        };
      }, {})
    : [];

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

  const [isMigration, setIsMigration] = useState(false);
  const [insertedRows, setInsertedRows] = useState(0);
  const [values, setValues] = useState<Record<ColumnName, RowValues>>({});

  const { data: metadata } = useMetadata();
  const tableMetadata = getTableMetadata(
    dataSourceName,
    tableName,
    metadata?.metadata
  );

  const { data: migrationMode } = useMigrationMode();
  const { data: readOnlyMode } = useReadOnlyMode();
  const { data: schemas, isLoading: schemasIsLoading } = useSchemas({
    dataSourceName,
    schemaName,
  });

  const tablesWithEnumRelations = getTableWithEnumRelations(
    dataSourceName,
    schemaName,
    metadata?.metadata
  );

  const { data: tableEnums } = useTableEnums({
    tables: tablesWithEnumRelations,
    dataSourceName,
  });
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

  const enumOptions = formatEnumOptions(tableEnums);

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

    dispatch(
      insertItem(
        tableName,
        currentRow ? { ...currentRow, ...inputValues } : inputValues,
        isMigration
      )
    ).then(() => {
      nextInsert();
    });
  };

  // --- Insert section end ---

  if (schemasIsLoading) return <p>Loading...</p>;

  return (
    <TableInsertItems
      isEnum={!!tableMetadata?.is_enum}
      toggleMigrationCheckBox={toggleMigrationCheckBox}
      onColumnUpdate={onColumnUpdate}
      isMigration={isMigration}
      dispatch={dispatch}
      tableName={tableName}
      currentSchema={schemaName}
      clone={currentRow}
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
