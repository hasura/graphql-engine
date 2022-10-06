import React from 'react';
import { AppDispatch } from '@/store';
import { Button } from '@/new-components/Button';
import { RightContainer } from '@/components/Common/Layout/RightContainer';
import { ordinalColSort } from '../utils';
import { NotFoundError } from '../../../Error/PageNotFound';
import {
  findTable,
  isFeatureSupported,
  generateTableDef,
  Table,
} from '../../../../dataSources';
import globals from '../../../../Globals';
import { CLI_CONSOLE_MODE } from '../../../../constants';
import FeatureDisabled from '../FeatureDisabled';
import {
  TableInsertRowItem,
  TableInsertRowItemProps,
} from './TableInsertRowItem';
import MigrationCheckbox from './MigrationCheckbox';
import ReloadEnumValuesButton from '../Common/Components/ReloadEnumValuesButton';
import TableHeader from '../TableCommon/TableHeader';

type AlertProps = {
  lastError: TableInsertItemsProps['lastError'];
  lastSuccess: TableInsertItemsProps['lastSuccess'];
};

const Alert = ({ lastError, lastSuccess }: AlertProps) => {
  if (lastError) {
    return (
      <div className="hidden alert alert-danger" role="alert">
        Error: {JSON.stringify(lastError)}
      </div>
    );
  }
  if (lastSuccess) {
    return (
      <div className="hidden alert alert-success" role="alert">
        Inserted! <br /> {JSON.stringify(lastSuccess)}
      </div>
    );
  }
  return null;
};

type TableInsertItemsProps = {
  tableName: string;
  currentSchema: string;
  clone: Record<string, unknown>;
  schemas: Table[];
  migrationMode: boolean;
  readOnlyMode: boolean;
  count: number;
  enumOptions: any;
  currentSource: string;
  onClickSave: (e: any) => void;
  onClickClear: () => void;
  toggleMigrationCheckBox: () => void;
  isMigration: boolean;
  onColumnUpdate: TableInsertRowItemProps['onColumnUpdate'];
  dispatch: AppDispatch;
  lastError: Record<any, any>;
  lastSuccess: Record<any, any>;
  buttonText: string;
};

export const TableInsertItems = ({
  tableName,
  currentSchema,
  clone,
  schemas,
  migrationMode,
  readOnlyMode,
  count,
  enumOptions,
  currentSource,
  onClickSave,
  onClickClear,
  toggleMigrationCheckBox,
  isMigration,
  onColumnUpdate,
  dispatch,
  lastError,
  lastSuccess,
  buttonText,
}: TableInsertItemsProps) => {
  if (!isFeatureSupported('tables.insert.enabled')) {
    return (
      <FeatureDisabled
        tab="insert"
        tableName={tableName}
        schemaName={currentSchema}
      />
    );
  }

  const currentTable = findTable(
    schemas,
    generateTableDef(tableName, currentSchema)
  );

  if (!currentTable) {
    throw new NotFoundError();
  }

  const isCLIMode = globals.consoleMode === CLI_CONSOLE_MODE;

  const columns = currentTable?.columns.sort(ordinalColSort) || [];

  return (
    <>
      <RightContainer>
        <div>
          <TableHeader
            count={count}
            dispatch={dispatch}
            table={currentTable}
            source={currentSource}
            tabName="insert"
            migrationMode={migrationMode}
            readOnlyMode={readOnlyMode}
            isCountEstimated={false}
          />
          <br />
          <div>
            <div className="justify-center w-9/12 pl-md">
              <form id="insertForm">
                <div className="flex flex-col pt-sm">
                  {columns.map((column, index) => (
                    <TableInsertRowItem
                      key={column?.column_name}
                      column={column}
                      onColumnUpdate={onColumnUpdate}
                      enumOptions={enumOptions}
                      baseCopyRow={clone}
                      index={index.toString()}
                    />
                  ))}
                </div>
                <div className="my-sm">
                  <MigrationCheckbox
                    onChange={toggleMigrationCheckBox}
                    isChecked={isMigration}
                    isCLIMode={isCLIMode}
                  />
                </div>
                <div className="flex items-center">
                  <div className="mr-md">
                    <Button
                      type="submit"
                      mode="primary"
                      onClick={onClickSave}
                      data-test="insert-save-button"
                    >
                      {buttonText}
                    </Button>
                  </div>
                  <div className="mr-md">
                    <Button onClick={onClickClear} data-test="clear-button">
                      Clear
                    </Button>
                  </div>
                  {currentTable.is_enum ? (
                    <ReloadEnumValuesButton dispatch={dispatch} />
                  ) : null}
                </div>
              </form>
            </div>
            <div className="w-3/12">
              <Alert lastError={lastError} lastSuccess={lastSuccess} />
            </div>
          </div>
          <br />
          <br />
        </div>
      </RightContainer>
    </>
  );
};
