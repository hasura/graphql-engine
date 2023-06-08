import React from 'react';
import { FaFlask } from 'react-icons/fa';
import {
  isFeatureSupported,
  driverToLabel,
  currentDriver,
  getTableCustomColumnNames,
  Table,
} from '../../../../dataSources';
import type { AppDispatch } from '../../../../store';
import { IconTooltip } from '../../../../new-components/Tooltip';
import { NotSupportedNote } from '../../../Common/NotSupportedNote';
import { Button } from '../../../../new-components/Button';
import {
  primaryKeyDescription,
  foreignKeyDescription,
  uniqueKeyDescription,
  checkConstraintsDescription,
  indexFieldsDescription,
} from '../Common/TooltipMessages';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import TableHeader from '../TableCommon/TableHeader';
import EnumsSection, {
  EnumTableModifyWarning,
} from '../Common/Components/EnumsSection';
import { setUniqueKeys } from './ModifyActions';
import TableCommentEditor from './TableCommentEditor';
import ColumnEditorList from './ColumnEditorList';
import ColumnCreator from './ColumnCreator';
import PrimaryKeyEditor from './PrimaryKeyEditor';
import ForeignKeyEditor from './ForeignKeyEditor';
import UniqueKeyEditor from './UniqueKeyEditor';
import CheckConstraints from './CheckConstraints';
import IndexFields from './IndexFields';
import PartitionInfo from './PartitionInfo';
import TriggerEditorList from './TriggerEditorList';
import ConnectedComputedFields from './ComputedFields';
import RootFields from './RootFields';
import {
  ApolloFederationSupport,
  ApolloFederationSupportProps,
} from '../Common/Components/ApolloFederationSupport';
import { useModifyTableSupportedFeatures } from './hooks/useModifyTableSupportedFeatures';
import { ColumnName } from '../TableCommon/DataTableRowItem.types';
import {
  CheckConstraintsType,
  ColumnEdit,
  DbDataType,
  DbFunctionName,
  ForeignKey,
  TypeAvailable,
  TypeCategory,
  TypeDescription,
  TypeDisplayName,
} from './ModifyTable.types';

export type ModifyTableProps = {
  allTables: Table[];
  checkConstraintsModify: CheckConstraintsType[];
  columnDefaultFunctions: Record<DbDataType, DbFunctionName[]>;
  columnEdit: ColumnEdit;
  currentSchema: string;
  currentSource: string;
  dataTypeIndexMap: Record<DbDataType, string[]>;
  dataTypes: [TypeAvailable, TypeDisplayName, TypeDescription, TypeCategory][];
  dispatch: AppDispatch;
  existingForeignKeys: ForeignKey[];
  fkModify: ForeignKey[];
  migrationMode: boolean;
  onDeleteTable: () => void;
  onToggleApolloFederation: ApolloFederationSupportProps['toggleApolloFederation'];
  onToggleEnum: () => void;
  onUntrackTable: () => void;
  orderedColumns: { name: ColumnName; index: number }[];
  pkModify: string[];
  postgresVersion: string;
  readOnlyMode: boolean;
  schemaList: string[];
  table: Table;
  tableCommentEdit: { enabled: boolean; editedValue: string | null };
  tableEnum: { loading: boolean };
  tableName: string;
  uniqueKeyModify: number[][];
  validTypeCasts: Record<DbDataType, string[]>;
};

export const ModifyTable: React.VFC<ModifyTableProps> = ({
  allTables,
  checkConstraintsModify,
  columnDefaultFunctions,
  columnEdit,
  currentSchema,
  currentSource,
  dataTypeIndexMap,
  dataTypes,
  dispatch,
  existingForeignKeys,
  fkModify,
  migrationMode,
  onDeleteTable,
  onToggleApolloFederation,
  onToggleEnum,
  onUntrackTable,
  orderedColumns,
  pkModify,
  postgresVersion,
  readOnlyMode,
  schemaList,
  table,
  tableCommentEdit,
  tableEnum,
  tableName,
  uniqueKeyModify,
  validTypeCasts,
}) => {
  const {
    areComputedFieldSupported,
    areTriggersSupported,
    isCheckConstraintsEditSupported,
    isCheckConstraintsViewSupported,
    isColumnsEditSupported,
    isColumnsViewSupported,
    isCommentsEditSupported,
    isCommentsViewSupported,
    isCustomGqlRootSupported,
    isForeignKeysEditSupported,
    isIndexViewSupported,
    isModifySupported,
    isPrimaryKeysEditSupported,
    isPrimaryKeysViewSupported,
    isReadOnlySupported,
    isSetAsEnumSupported,
    isUniqueKeysEditSupported,
    isUntrackSupported,
  } = useModifyTableSupportedFeatures();

  return (
    <RightContainer>
      <div className="bootstrap-jail">
        <TableHeader
          dispatch={dispatch}
          table={table}
          source={currentSource}
          tabName="modify"
          migrationMode={migrationMode}
          readOnlyMode={readOnlyMode}
          count={undefined}
          isCountEstimated={undefined}
        />
        <br />
        <div>
          <div>
            {isReadOnlySupported && (
              <div className="py-sm px-sm bg-gray-200 rounded-md mb-md">
                <p className="mb-xs font-bold">
                  <FaFlask aria-hidden="true" /> Coming soon for{' '}
                  {driverToLabel[currentDriver]}
                </p>
                <p className="m-0">
                  This page is currently read-only, but we&apos;re actively
                  working on making it available for the Console.
                </p>
              </div>
            )}
            {isCommentsViewSupported && (
              <div className="w-full sm:w-6/12 mb-lg">
                <EnumTableModifyWarning isEnum={table.is_enum} />

                <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
                  Table Comments
                </h4>
                <TableCommentEditor
                  tableComment={table.comment}
                  tableCommentEdit={tableCommentEdit}
                  tableType="TABLE"
                  dispatch={dispatch}
                  readOnly={!isCommentsEditSupported}
                />
              </div>
            )}

            <div className="w-full sm:w-full">
              <h3 className="text-sm tracking-widest text-gray-400 uppercase font-semibold mb-sm">
                Configure Fields
              </h3>

              {isColumnsViewSupported && (
                <>
                  <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
                    Columns
                  </h4>
                  <ColumnEditorList
                    validTypeCasts={validTypeCasts}
                    dataTypeIndexMap={dataTypeIndexMap}
                    tableSchema={table}
                    columnEdit={columnEdit}
                    dispatch={dispatch}
                    readOnlyMode={!isColumnsEditSupported}
                    currentSchema={currentSchema}
                    columnDefaultFunctions={columnDefaultFunctions}
                    customColumnNames={getTableCustomColumnNames(table)}
                  />
                </>
              )}
            </div>

            <div className="w-full mb-lg">
              {isColumnsEditSupported && (
                <ColumnCreator
                  dispatch={dispatch}
                  tableName={tableName || ''}
                  dataTypes={dataTypes}
                  validTypeCasts={validTypeCasts}
                  columnDefaultFunctions={columnDefaultFunctions}
                  postgresVersion={postgresVersion}
                />
              )}
            </div>

            <h3 className="text-sm tracking-widest text-gray-400 uppercase font-semibold mb-sm">
              Table Properties
            </h3>

            {isPrimaryKeysViewSupported && (
              <div className="w-full sm:w-6/12 mb-md">
                <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
                  Primary Key
                  <IconTooltip message={primaryKeyDescription} />
                </h4>
                <PrimaryKeyEditor
                  tableSchema={table}
                  readOnlyMode={!isPrimaryKeysEditSupported}
                  pkModify={pkModify}
                  dispatch={dispatch}
                  currentSchema={currentSchema}
                />
              </div>
            )}

            {isFeatureSupported('tables.modify.foreignKeys.view') && (
              <div className="w-full sm:w-8/12 mb-md">
                <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
                  Foreign Keys
                  <IconTooltip message={foreignKeyDescription} />
                </h4>
                <ForeignKeyEditor
                  tableSchema={table}
                  allSchemas={allTables}
                  dispatch={dispatch}
                  schemaList={schemaList}
                  fkModify={fkModify}
                  orderedColumns={orderedColumns}
                  existingForeignKeys={existingForeignKeys}
                  readOnlyMode={!isForeignKeysEditSupported}
                />
              </div>
            )}

            {isFeatureSupported('tables.modify.uniqueKeys.view') && (
              <div className="w-full sm:w-6/12 mb-md">
                <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
                  Unique Keys
                  <IconTooltip message={uniqueKeyDescription} />
                </h4>
                <UniqueKeyEditor
                  tableSchema={table}
                  dispatch={dispatch}
                  uniqueKeys={uniqueKeyModify}
                  setUniqueKeys={setUniqueKeys}
                  readOnlyMode={!isUniqueKeysEditSupported}
                />
              </div>
            )}
            {isCheckConstraintsViewSupported && (
              <div className="w-full sm:w-6/12 mb-md">
                <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
                  Check Constraints
                  <IconTooltip message={checkConstraintsDescription} />
                </h4>
                <NotSupportedNote unsupported={['mysql']} />
                <CheckConstraints
                  constraints={table.check_constraints}
                  checkConstraintsModify={checkConstraintsModify}
                  dispatch={dispatch}
                  readOnlyMode={!isCheckConstraintsEditSupported}
                />
              </div>
            )}
            {isIndexViewSupported ? (
              <div className="w-full sm:w-6/12 mb-md">
                <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
                  Indexes
                  <IconTooltip message={indexFieldsDescription} />
                </h4>
                <IndexFields tableSchema={table} />
              </div>
            ) : null}

            {table.table_type === 'PARTITIONED TABLE' && (
              <PartitionInfo table={table} dispatch={dispatch} />
            )}

            {areTriggersSupported && (
              <div className="w-full sm:w-6/12 mb-lg">
                <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
                  Triggers
                </h4>
                <NotSupportedNote unsupported={['mysql']} />
                <TriggerEditorList tableSchema={table} dispatch={dispatch} />
              </div>
            )}

            <h3 className="text-sm tracking-widest text-gray-400 uppercase font-semibold mb-sm">
              GraphQL Features
            </h3>

            {areComputedFieldSupported && (
              <div className="w-full sm:w-6/12 mb-md">
                <ConnectedComputedFields tableSchema={table} />
              </div>
            )}
            {isCustomGqlRootSupported && (
              <div className="w-full sm:w-8/12 mb-md">
                <RootFields tableSchema={table} />
              </div>
            )}
            {isSetAsEnumSupported && (
              <EnumsSection
                isEnum={table.is_enum}
                toggleEnum={() => onToggleEnum()}
                loading={tableEnum.loading}
              />
            )}

            <ApolloFederationSupport
              toggleApolloFederation={onToggleApolloFederation}
              isApolloFederationSupported={
                table?.is_apollo_federation_supported || false
              }
            />

            <div className="mb-lg">
              {isUntrackSupported && (
                <Button
                  type="submit"
                  className="mr-sm"
                  size="sm"
                  onClick={() => onUntrackTable()}
                  data-test="untrack-table"
                >
                  Untrack Table
                </Button>
              )}
              {isModifySupported && (
                <Button
                  type="submit"
                  mode="destructive"
                  size="sm"
                  onClick={() => onDeleteTable()}
                  data-test="delete-table"
                >
                  Delete table
                </Button>
              )}
            </div>
          </div>
        </div>
      </div>
    </RightContainer>
  );
};
