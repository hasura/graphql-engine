import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  findTable,
  generateTableDef,
  isFeatureSupported,
  Table,
} from '../../../../dataSources';
import { useAppDispatch, useAppSelector } from '../../../../storeHooks';
import React, { useEffect } from 'react';
import { getAllDataTypeMap } from '../Common/utils';
import {
  fetchColumnTypeInfo,
  fetchFunctionInit,
  RESET_COLUMN_TYPE_INFO,
  setTable,
} from '../DataActions';
import FeatureDisabled from '../FeatureDisabled';
import { useForeignKeys } from './hooks/useForeignKeys';
import {
  deleteTableSql,
  RESET,
  toggleAsApolloFederation,
  toggleTableAsEnum,
  untrackTableSql,
} from './ModifyActions';
import { ModifyTable, ModifyTableProps } from './ModifyTable';

type ColumnEdit = {
  comment: string;
  customFieldName: string;
  default: string;
  display_type_name: string;
  isArrayDataType: boolean;
  isIdentity: boolean;
  isNullable: boolean;
  isOnlyPrimaryKey: boolean;
  isUnique: boolean;
  name: string;
  pkConstraint: string;
  schemaName: string;
  tableName: string;
  type: string;
};

type FkModify = {
  colMappings: any[];
  constraintName: string;
  onDelete: string;
  onUpdate: string;
  refSchemaName: string;
  refTableName: string;
};

type TableModify = {
  columnEdit: Record<string, ColumnEdit>;
  pkModify: string[];
  fkModify: FkModify[];
  tableEnum: any;
  tableCommentEdit: any;
  checkConstraintsModify: any;
  uniqueKeyModify: any;
};

type Props = {
  params: {
    table: string;
  };
};

export const ModifyTableContainer: React.VFC<Props> = ({ params }) => {
  const dispatch = useAppDispatch();
  const tableName = params.table;

  useEffect(() => {
    dispatch({ type: RESET });
    dispatch(setTable(tableName));

    if (!isFeatureSupported('tables.modify.readOnly')) {
      dispatch(fetchColumnTypeInfo());
    }
    dispatch(fetchFunctionInit());

    return () => {
      dispatch({
        type: RESET_COLUMN_TYPE_INFO,
      });
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [tableName]);

  const allTables: Table[] = useAppSelector(store => store.tables.allSchemas);
  const migrationMode: boolean = useAppSelector(
    store => store.main.migrationMode
  );
  const readOnlyMode = useAppSelector(store => store.main.readOnlyMode);
  const currentSchema = useAppSelector(store => store.tables.currentSchema);

  const tableModify: TableModify = useAppSelector(store => store.tables.modify);

  const {
    columnEdit,
    pkModify,
    fkModify,
    tableEnum,
    tableCommentEdit,
    checkConstraintsModify,
    uniqueKeyModify,
  } = tableModify;

  const dataTypes = useAppSelector(store => store.tables.columnDataTypes);
  const columnDefaultFunctions = useAppSelector(
    store => store.tables.columnDefaultFunctions
  );
  const validTypeCasts = useAppSelector(store => store.tables.columnTypeCasts);
  const schemaList = useAppSelector(store => store.tables.schemaList);
  const postgresVersion = useAppSelector(store => store.main.postgresVersion);
  const currentSource = useAppSelector(store => store.tables.currentDataSource);

  const table = findTable(
    allTables,
    generateTableDef(tableName, currentSchema)
  );

  const { orderedColumns, existingForeignKeys } = useForeignKeys({ table });

  if (!table) {
    return null;
  }

  const isModifyEnabled = isFeatureSupported('tables.modify.enabled');
  if (!isModifyEnabled) {
    return (
      <FeatureDisabled
        tab="modify"
        tableName={tableName}
        schemaName={currentSchema}
      />
    );
  }

  const dataTypeIndexMap = getAllDataTypeMap(
    dataTypes
  ) as ModifyTableProps['dataTypeIndexMap'];

  const onUntrackTable = () => {
    const confirmMessage = `This will remove the table "${tableName}" from the GraphQL schema`;
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      dispatch(untrackTableSql(tableName));
    }
  };

  const onDeleteTable = () => {
    const confirmMessage = `This will permanently delete the table "${tableName}" from the database`;
    const isOk = getConfirmation(confirmMessage, true, tableName);
    if (isOk) {
      dispatch(deleteTableSql(tableName));
    }
  };

  const onToggleEnum = () => dispatch(toggleTableAsEnum(table?.is_enum));

  const onToggleApolloFederation = () =>
    dispatch(
      toggleAsApolloFederation(table?.is_apollo_federation_supported || false)
    );

  return (
    <ModifyTable
      allTables={allTables}
      checkConstraintsModify={checkConstraintsModify}
      columnDefaultFunctions={columnDefaultFunctions}
      columnEdit={columnEdit}
      currentSchema={currentSchema}
      currentSource={currentSource}
      dataTypeIndexMap={dataTypeIndexMap}
      dataTypes={dataTypes}
      dispatch={dispatch}
      existingForeignKeys={existingForeignKeys}
      fkModify={fkModify}
      migrationMode={migrationMode}
      onDeleteTable={onDeleteTable}
      onToggleApolloFederation={onToggleApolloFederation}
      onToggleEnum={onToggleEnum}
      onUntrackTable={onUntrackTable}
      orderedColumns={orderedColumns}
      pkModify={pkModify}
      postgresVersion={postgresVersion}
      readOnlyMode={readOnlyMode}
      schemaList={schemaList}
      table={table}
      tableCommentEdit={tableCommentEdit}
      tableEnum={tableEnum}
      tableName={tableName}
      uniqueKeyModify={uniqueKeyModify}
      validTypeCasts={validTypeCasts}
    />
  );
};
