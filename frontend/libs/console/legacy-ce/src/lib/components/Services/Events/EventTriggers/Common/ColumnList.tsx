import React, { useState } from 'react';
import { QualifiedTable } from '../../../../../metadata/types';
import { Button } from '../../../../../new-components/Button';
import { ETOperationColumn } from '../../types';
import { ColumnSelectionRadioButton } from './ColumnSelectionRadioButton';

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

  const [allColEnabled, setAllColEnabled] = useState(true);

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

  const handleToggleAllColumns = () => {
    if (allColEnabled) {
      const cols = operationColumns.map(col => {
        return {
          ...col,
          enabled: false,
        };
      });
      handleOperationsColumnsChange(cols);
      setAllColEnabled(false);
    } else {
      const cols = operationColumns.map(col => {
        return {
          ...col,
          enabled: true,
        };
      });
      handleOperationsColumnsChange(cols);
      setAllColEnabled(true);
    }
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
            <div>
              List of columns to select:
              <Button
                className="ml-2"
                size="sm"
                onClick={() => handleToggleAllColumns()}
              >
                Toggle All
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
