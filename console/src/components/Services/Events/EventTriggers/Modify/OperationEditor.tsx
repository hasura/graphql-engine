import React from 'react';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import {
  EventTrigger,
  EventTriggerOperation,
  ETOperationColumn,
  VoidCallback,
} from '../../types';
import {
  parseEventTriggerOperations,
  getETOperationColumns,
} from '../../utils';
import { Operations } from '../Common/Operations';
import { TableColumn } from '../../../../../dataSources/types';
import { ColumnSelectionRadioButton } from '../Common/ColumnSelectionRadioButton';
import { focusYellowRing } from '../../constants';

type OperationEditorProps = {
  currentTrigger: EventTrigger;
  allTableColumns: TableColumn[];
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
    allTableColumns,
    save,
    currentTrigger,
    operations,
    operationColumns,
    setOperations,
    setOperationColumns,
    isAllColumnChecked,
    handleColumnRadioButton,
  } = props;
  const etDef = currentTrigger.configuration.definition;
  const existingOps = parseEventTriggerOperations(etDef);
  const existingOpColumns = getETOperationColumns(
    etDef.update ? etDef.update.columns : [],
    allTableColumns
  );

  const reset = () => {
    setOperations(existingOps);
    setOperationColumns(existingOpColumns);
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
            <div className={`w-full p-0 m-sm ${focusYellowRing}`}>
              <ColumnSelectionRadioButton
                isAllColumnChecked={isAllColumnChecked}
                handleColumnRadioButton={handleColumnRadioButton}
                readOnly={readOnly}
              />
            </div>
            <>
              {!isAllColumnChecked ? (
                <div className="w-fill p-0">
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
                        className="m-sm float-left p-0 pointer-cursor"
                        key={col.name}
                        onChange={toggle}
                      >
                        <input
                          type="checkbox"
                          className={`!mr-xs cursor-pointer ${focusYellowRing}`}
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
    <div className="w-full pb-md">
      <div>
        <h4 className="font-bold text-md mb-md">Trigger Operations</h4>
        <Editor
          editorCollapsed={collapsed}
          editorExpanded={expanded}
          property="ops"
          service="modify-trigger"
          saveFunc={save}
          expandCallback={reset}
        />
      </div>
    </div>
  );
};
