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

type OperationEditorProps = {
  currentTrigger: EventTrigger;
  allTableColumns: TableColumn[];
  operations: Record<EventTriggerOperation, boolean>;
  setOperations: (o: Record<EventTriggerOperation, boolean>) => void;
  operationColumns: ETOperationColumn[];
  setOperationColumns: (operationColumns: ETOperationColumn[]) => void;
  styles: Record<string, string>;
  save: (success: VoidCallback, error: VoidCallback) => void;
  isAllColumnChecked: boolean;
  handleColumnRadioButton: () => void;
};

export const OperationEditor: React.FC<OperationEditorProps> = props => {
  const {
    allTableColumns,
    styles,
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
    <div className={styles.modifyOps}>
      <label className="block text-gray-600 font-medium mb-sm">
        Trigger Method
      </label>
      <div
        className={`${styles.modifyOpsCollapsedContent} ${styles.add_mar_bottom_mid} col-md-12 ${styles.padd_remove}`}
      >
        <Operations
          selectedOperations={ops}
          setOperations={setOperations}
          readOnly={readOnly}
          tableName={currentTrigger.table_name}
        />
      </div>
      <div>
        <label className="block text-gray-600 font-medium mb-xs">
          Trigger Columns
        </label>
        <p className="text-sm text-gray-600 mb-sm">
          Trigger columns to listen to for updates.
        </p>
      </div>
      <div className={styles.modifyOpsCollapsedContent}>
        {ops.update ? (
          <>
            <div className={`col-md-12 ${styles.padd_remove} checkbox`}>
              <ColumnSelectionRadioButton
                isAllColumnChecked={isAllColumnChecked}
                handleColumnRadioButton={handleColumnRadioButton}
                readOnly={readOnly}
              />
            </div>
            <>
              {!isAllColumnChecked ? (
                <div className={`col-md-12 ${styles.padd_remove}`}>
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
                        className={`${styles.opsCheckboxWrapper} ${styles.columnListElement} ${styles.padd_remove} ${styles.cursorPointer}`}
                        key={col.name}
                        onChange={toggle}
                      >
                        <input
                          type="checkbox"
                          className={`${styles.opsCheckboxDisabled} ${styles.cursorPointer} legacy-input-fix`}
                          checked={col.enabled}
                          disabled={readOnly}
                          readOnly
                        />
                        {col.name}
                        <small className={styles.addPaddSmall}>
                          {' '}
                          ({col.type})
                        </small>
                      </label>
                    );
                  })}
                </div>
              ) : null}
            </>
          </>
        ) : (
          <div
            className={`col-md-12 ${styles.padd_remove} ${styles.modifyOpsCollapsedtitle}`}
          >
            <i>Applicable only if update operation is selected.</i>
          </div>
        )}
      </div>
    </div>
  );

  const collapsed = () => renderEditor(existingOps, existingOpColumns, true);

  const expanded = () => renderEditor(operations, operationColumns, false);

  return (
    <div className={`${styles.container} ${styles.borderBottom}`}>
      <div className={styles.modifySection}>
        <h4 className={styles.modifySectionHeading}>Trigger Operations</h4>
        <Editor
          editorCollapsed={collapsed}
          editorExpanded={expanded}
          styles={styles}
          property="ops"
          service="modify-trigger"
          saveFunc={save}
          expandCallback={reset}
        />
      </div>
    </div>
  );
};
