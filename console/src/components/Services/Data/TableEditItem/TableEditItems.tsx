import React from 'react';
import { AppDispatch } from '@/store';
import { Button } from '@/new-components/Button';
import { RightContainer } from '@/components/Common/Layout/RightContainer';
import { ordinalColSort } from '../utils';
import { NotFoundError } from '../../../Error/PageNotFound';
import { findTable, generateTableDef, Table } from '../../../../dataSources';
import {
  DataTableRowItem,
  DataTableRowItemProps,
} from '../TableCommon/DataTableRowItem';
import ReloadEnumValuesButton from '../Common/Components/ReloadEnumValuesButton';
import TableHeader from '../TableCommon/TableHeader';

type AlertProps = {
  lastError: TableEditItemsProps['lastError'];
  lastSuccess: TableEditItemsProps['lastSuccess'];
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
        Updated! <br /> {JSON.stringify(lastSuccess)}
      </div>
    );
  }
  return null;
};

type TableEditItemsProps = {
  oldItem: Record<string, string>;
  ongoingRequest: boolean;
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
  onColumnUpdate: DataTableRowItemProps['onColumnUpdate'];
  dispatch: AppDispatch;
  lastError: Record<any, any>;
  lastSuccess: Record<any, any>;
  buttonText: string;
};

export const TableEditItems = ({
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
  onColumnUpdate,
  dispatch,
  lastError,
  lastSuccess,
  buttonText,
  oldItem,
}: TableEditItemsProps) => {
  const currentTable = findTable(
    schemas,
    generateTableDef(tableName, currentSchema)
  );

  if (!currentTable) {
    throw new NotFoundError();
  }

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
            tabName="edit"
            migrationMode={migrationMode}
            readOnlyMode={readOnlyMode}
            isCountEstimated={false}
          />
          <br />
          <div>
            <div className="justify-center w-9/12 pl-md">
              <form id="updateForm">
                <div className="flex flex-col pt-sm">
                  {columns.map((column, index) => (
                    <DataTableRowItem
                      defaultValue={
                        oldItem ? oldItem[column?.column_name] : undefined
                      }
                      key={column?.column_name}
                      column={column}
                      onColumnUpdate={onColumnUpdate}
                      enumOptions={enumOptions}
                      baseCopyRow={clone}
                      index={index.toString()}
                    />
                  ))}
                </div>
                <div className="flex items-center my-sm">
                  <div className="mr-md">
                    <Button
                      type="submit"
                      mode="primary"
                      onClick={onClickSave}
                      data-test="edit-save-button"
                    >
                      {buttonText}
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
