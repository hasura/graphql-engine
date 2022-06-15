import React from 'react';
import { QualifiedTable } from '@/metadata/types';
import { ETOperationColumn } from '../../types';
import { ColumnSelectionRadioButton } from './ColumnSelectionRadioButton';
import Button from '../../../../Common/Button';

type ColumnListProps = {
  operationColumns: ETOperationColumn[];
  table: QualifiedTable;
  isAllColumnChecked: boolean;
  readOnlyMode: boolean;
  handleToggleAllColumn: () => void;
  handleOperationsColumnsChange: (oc: ETOperationColumn[]) => void;
};

const ColumnList: React.FC<ColumnListProps> = props => {
  const {
    operationColumns,
    table,
    isAllColumnChecked,
    readOnlyMode,
    handleToggleAllColumn,
    handleOperationsColumnsChange,
  } = props;

  if (!table.name) {
    return <i>Select a table first to get column list</i>;
  }

  const handleToggleColumn = (opCol: ETOperationColumn) => {
    const newCols = operationColumns.map(o => {
      return {
        ...o,
        enabled: o.name === opCol.name ? !o.enabled : o.enabled,
      };
    });
    handleOperationsColumnsChange(newCols);
  };

  const handleSelectAllColumns = () => {
    const newCols = operationColumns.map(o => {
      return {
        ...o,
        enabled: true,
      };
    });
    handleOperationsColumnsChange(newCols);
  };

  const handleUnselectAllColumns = () => {
    const newCols = operationColumns.map(o => {
      return {
        ...o,
        enabled: false,
      };
    });
    handleOperationsColumnsChange(newCols);
  };

  return (
    <>
      <ColumnSelectionRadioButton
        isAllColumnChecked={isAllColumnChecked}
        handleColumnRadioButton={handleToggleAllColumn}
        readOnly={readOnlyMode}
      />
      <div className="mt-sm">
        {!isAllColumnChecked ? (
          <>
            <div>List of columns to select:</div>
            <div className="flex space-x-2 mt-0.5">
              <Button onClick={handleSelectAllColumns} color="white" size="sm">
                Select all
              </Button>
              <Button
                onClick={handleUnselectAllColumns}
                color="white"
                size="sm"
              >
                Unselect all
              </Button>
            </div>
            {operationColumns.map(opCol => (
              <div key={opCol.name} className="p-0 float-left mr-xl">
                <div className="checkbox">
                  <label className="cursor-pointer">
                    <input
                      type="checkbox"
                      checked={opCol.enabled}
                      onChange={() => handleToggleColumn(opCol)}
                      className="cursor-pointer"
                      data-test="select-column"
                    />
                    {opCol.name}
                  </label>
                </div>
              </div>
            ))}
          </>
        ) : null}
      </div>
    </>
  );
};

export default ColumnList;
