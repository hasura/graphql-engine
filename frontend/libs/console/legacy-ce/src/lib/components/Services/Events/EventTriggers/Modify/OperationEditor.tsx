import React, { useState } from 'react';
import { Button } from '../../../../../new-components/Button';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import {
  EventTrigger,
  EventTriggerOperation,
  ETOperationColumn,
  VoidCallback,
  DatabaseInfo,
} from '../../types';
import {
  parseEventTriggerOperations,
  getETOperationColumns,
} from '../../utils';
import { Operations } from '../Common/Operations';
import { ColumnSelectionRadioButton } from '../Common/ColumnSelectionRadioButton';
import { focusYellowRing } from '../../constants';

type OperationEditorProps = {
  currentTrigger: EventTrigger;
  databaseInfo: DatabaseInfo;
  operations: Record<EventTriggerOperation, boolean>;
  setOperations: (o: Record<EventTriggerOperation, boolean>) => void;
  operationColumns: ETOperationColumn[];
  setOperationColumns: (operationColumns: ETOperationColumn[]) => void;
  save: (success: VoidCallback, error: VoidCallback) => void;
  isAllColumnChecked: boolean;
  handleColumnRadioButton: () => void;
};

export const OperationEditor: React.FC<OperationEditorProps> = props => {
  const {
    databaseInfo,
    save,
    currentTrigger,
    operations,
    operationColumns,
    setOperations,
    setOperationColumns,
    isAllColumnChecked,
    handleColumnRadioButton,
  } = props;
  const [allColEnabled, setAllColEnabled] = useState(true);
  const etDef = currentTrigger.configuration.definition;
  const existingOps = parseEventTriggerOperations(etDef);
  const columnInfo =
    databaseInfo?.[currentTrigger.schema_name]?.[currentTrigger.table_name] ??
    [];
  const existingOpColumns = getETOperationColumns(
    etDef.update ? etDef.update.columns : [],
    columnInfo
  );

  const reset = () => {
    setOperations(existingOps);
    setOperationColumns(existingOpColumns);
  };

  const handleToggleAllColumns = () => {
    if (allColEnabled) {
      const cols = operationColumns.map(o => {
        return {
          ...o,
          enabled: false,
        };
      });
      setOperationColumns(cols);
      setAllColEnabled(false);
    } else {
      const cols = operationColumns.map(o => {
        return {
          ...o,
          enabled: true,
        };
      });
      setOperationColumns(cols);
      setAllColEnabled(true);
    }
  };

  const renderEditor = (
    ops: Record<EventTriggerOperation, boolean>,
    opCols: ETOperationColumn[],
    readOnly: boolean
  ) => (
    <div>
      <label className="block text-gray-600 font-medium mb-sm">
        Trigger Method
      </label>
      <div className="flex">
        <div className="mb-sm w-full p-0">
          <Operations
            selectedOperations={ops}
            setOperations={setOperations}
            readOnly={readOnly}
            tableName={currentTrigger.table_name}
          />
        </div>
      </div>
      <div>
        <label className="block text-gray-600 font-medium mb-xs">
          Trigger Columns
        </label>
        <p className="text-sm text-gray-600 mb-sm">
          Trigger columns to listen to for updates.
        </p>
      </div>
      <div>
        {ops.update ? (
          <>
            <div className={`w-full p-0 mr-md mt-md ${focusYellowRing}`}>
              <ColumnSelectionRadioButton
                isAllColumnChecked={isAllColumnChecked}
                handleColumnRadioButton={handleColumnRadioButton}
                readOnly={readOnly}
              />
            </div>
            {!isAllColumnChecked ? (
              <div className="w-full p-0 mt-sm">
                <div>
                  List of columns to select:
                  <Button
                    className="ml-2"
                    size="sm"
                    onClick={() => handleToggleAllColumns()}
                    disabled={readOnly}
                  >
                    Toggle All
                  </Button>
                </div>
                {opCols.map(col => {
                  const toggle = () => {
                    if (!readOnly) {
                      const newCols = opCols.map(oc => {
                        return {
                          ...oc,
                          enabled:
                            col.name === oc.name ? !oc.enabled : oc.enabled,
                        };
                      });
                      setOperationColumns(newCols);
                    }
                  };
                  return (
                    <label
                      className="mr-md my-md p-0 pointer-cursor"
                      key={col.name}
                      onChange={toggle}
                    >
                      <input
                        type="checkbox"
                        name={`column-${col.name}`}
                        className={`!mr-xs cursor-pointer ${focusYellowRing} disabled:bg-gray-200 disabled:cursor-not-allowed disabled:text-gray-200 border-gray-200 rounded-sm bg-white`}
                        checked={col.enabled}
                        disabled={readOnly}
                        readOnly
                      />
                      {col.name}
                      <small className="p-xs"> ({col.type})</small>
                    </label>
                  );
                })}
              </div>
            ) : null}
          </>
        ) : (
          <div className="w-full p-0">
            <i>Applicable only if update operation is selected.</i>
          </div>
        )}
      </div>
    </div>
  );

  const collapsed = () => renderEditor(existingOps, existingOpColumns, true);

  const expanded = () => renderEditor(operations, operationColumns, false);

  return (
    <div className="w-full">
      <div>
        <h4 className="font-bold text-lg mb-md">Trigger Operations</h4>
        <Editor
          editorCollapsed={collapsed}
          editorExpanded={expanded}
          property="ops"
          service="modify-trigger"
          saveFunc={save}
          expandCallback={reset}
          dataTest="edit-operations"
        />
      </div>
    </div>
  );
};
