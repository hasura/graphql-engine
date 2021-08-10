import React from 'react';
import Editor from '../../../../Common/Layout/ExpandableEditor/Editor';
import Tooltip from '../../../../Common/Tooltip/Tooltip';
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

import Operations from '../Common/Operations';
import { TableColumn } from '../../../../../dataSources/types';

type OperationEditorProps = {
  currentTrigger: EventTrigger;
  allTableColumns: TableColumn[];
  operations: Record<EventTriggerOperation, boolean>;
  setOperations: (o: Record<EventTriggerOperation, boolean>) => void;
  operationColumns: ETOperationColumn[];
  setOperationColumns: (operationColumns: ETOperationColumn[]) => void;
  styles: Record<string, string>;
  save: (success: VoidCallback, error: VoidCallback) => void;
};

const OperationEditor = (props: OperationEditorProps) => {
  const {
    allTableColumns,
    styles,
    save,
    currentTrigger,
    operations,
    operationColumns,
    setOperations,
    setOperationColumns,
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
      <div
        className={`${styles.modifyOpsCollapsedContent} ${styles.add_mar_bottom_mid}`}
      >
        <div className={`col-md-12 ${styles.padd_remove}`}>
          <Operations
            selectedOperations={ops}
            setOperations={setOperations}
            readOnly={readOnly}
          />
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent}>
        <div className={`col-md-12 ${styles.padd_remove}`}>
          Listen columns for update:&nbsp;
        </div>
        <div className={`col-md-12 ${styles.padd_remove}`}>
          {ops.update ? (
            opCols.map(col => {
              const toggle = () => {
                if (!readOnly) {
                  const newCols = opCols.map(oc => {
                    return {
                      ...oc,
                      enabled: col.name === oc.name ? !oc.enabled : oc.enabled,
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
                  <small className={styles.addPaddSmall}> ({col.type})</small>
                </label>
              );
            })
          ) : (
            <div
              className={`col-md-12 ${styles.padd_remove} ${styles.modifyOpsCollapsedtitle}`}
            >
              <i>Applicable only if update operation is selected.</i>
            </div>
          )}
        </div>
      </div>
    </div>
  );

  const collapsed = () => renderEditor(existingOps, existingOpColumns, true);

  const expanded = () => renderEditor(operations, operationColumns, false);

  return (
    <div className={`${styles.container} ${styles.borderBottom}`}>
      <div className={styles.modifySection}>
        <h4 className={styles.modifySectionHeading}>
          Trigger Operations{' '}
          <Tooltip message="Edit operations and related columns" />
        </h4>
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

export default OperationEditor;
