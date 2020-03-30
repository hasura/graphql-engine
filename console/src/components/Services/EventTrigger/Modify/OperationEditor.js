import React from 'react';

import Editor from '../../../Common/Layout/ExpandableEditor/Editor';
import { toggleQueryType, toggleColumn, toggleManualType } from './Actions';
import {
  getTriggerOperations,
  triggerOperationMap,
  MANUAL_TRIGGER_VAR,
} from './utils';
import { ToolTip, Heading } from '../../../UIKit/atoms';

const OperationEditor = props => {
  const {
    definition,
    allTableColumns,
    styles,
    save,
    modifyTrigger,
    dispatch,
  } = props;

  const toggleOperation = upObj => {
    if (upObj.query === MANUAL_TRIGGER_VAR) {
      return toggleManualType(upObj);
    }
    return toggleQueryType(upObj);
  };

  const setValues = () => {
    /*
     * Loop through the keys in definition,
     * this object will have actual internal name.
     * No need to transform from display to internal name
     * */
    for (const queryType in definition) {
      /* If the definition[queryType] holds true
       * This will be false or undefined if `queryType` doesn't exist in the object or definition[queryType] is false.
       * */
      if (queryType in definition) {
        if (queryType !== MANUAL_TRIGGER_VAR) {
          dispatch(
            toggleOperation({
              query: queryType,
              columns: definition[queryType].columns,
              value: true,
            })
          );
        } else {
          dispatch(
            toggleOperation({
              query: queryType,
              value: definition[queryType],
            })
          );
        }
      }
    }
  };

  /*
   * Query types will have `CONSOLE_QUERY` only for version > 45
   *
   * */
  const operationTypes = getTriggerOperations();

  const renderOperation = (qt, i) => {
    const isChecked = Boolean(definition[triggerOperationMap[qt]]);

    return (
      <div
        className={
          styles.opsCheckboxWrapper + ' col-md-2 ' + styles.padd_remove
        }
        key={i}
      >
        <input
          type="checkbox"
          className={styles.opsCheckboxDisabled}
          checked={isChecked}
          disabled
        />
        {qt}
      </div>
    );
  };

  const collapsed = () => (
    <div className={styles.modifyOps}>
      <div className={styles.modifyOpsCollapsedContent}>
        <div className={'col-md-12 ' + styles.padd_remove}>
          {operationTypes.map((qt, i) => renderOperation(qt, i))}
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent}>
        <div className={'col-md-12 ' + styles.padd_remove}>
          Listen columns for update:&nbsp;
        </div>
        <div className={'col-md-12 ' + styles.padd_remove}>
          {definition.update ? (
            allTableColumns.map((col, i) => (
              <div
                className={`${styles.opsCheckboxWrapper} col-md-4 ${styles.padd_remove}`}
                key={i}
              >
                <input
                  type="checkbox"
                  className={styles.opsCheckboxDisabled}
                  checked={Boolean(
                    definition.update.columns.find(c => c === col.name)
                  )}
                  disabled
                />
                {col.name}
                <small className={styles.addPaddSmall}> ({col.type})</small>
              </div>
            ))
          ) : (
            <div
              className={
                'col-md-12 ' +
                styles.padd_remove +
                ' ' +
                styles.modifyOpsCollapsedtitle
              }
            >
              <i>Applicable only if update operation is selected.</i>
            </div>
          )}
        </div>
      </div>
    </div>
  );

  const expanded = () => (
    <div className={styles.modifyOpsPadLeft}>
      <div className={styles.modifyOpsCollapsedContent}>
        <div className={'col-md-12 ' + styles.padd_remove}>
          {operationTypes.map((qt, i) => (
            <div
              className={`${styles.opsCheckboxWrapper} col-md-2 ${styles.padd_remove} ${styles.cursorPointer}`}
              key={i}
              onClick={() => {
                dispatch(
                  toggleOperation({
                    query: triggerOperationMap[qt],
                    columns: allTableColumns.map(c => c.name),
                    value: !modifyTrigger.definition[triggerOperationMap[qt]],
                  })
                );
              }}
            >
              <input
                type="checkbox"
                className={`${styles.opsCheckbox} ${styles.cursorPointer}`}
                checked={Boolean(
                  modifyTrigger.definition[triggerOperationMap[qt]]
                )}
              />
              {qt}
            </div>
          ))}
        </div>
      </div>
      <div className={styles.modifyOpsCollapsedContent}>
        <div className={'col-md-12 ' + styles.padd_remove}>
          Listen columns for update:&nbsp;
        </div>
        <div className={'col-md-12 ' + styles.padd_remove}>
          {modifyTrigger.definition.update ? (
            allTableColumns.map((col, i) => (
              <div
                className={`${styles.opsCheckboxWrapper} col-md-4 ${styles.padd_remove} ${styles.cursorPointer}`}
                key={i}
                onClick={() => dispatch(toggleColumn('update', col.name))}
              >
                <input
                  type="checkbox"
                  className={`${styles.opsCheckbox} ${styles.cursorPointer}`}
                  checked={Boolean(
                    modifyTrigger.definition.update.columns.find(
                      c => c === col.name
                    )
                  )}
                />
                {col.name}
                <small className={styles.addPaddSmall}> ({col.type})</small>
              </div>
            ))
          ) : (
            <div
              className={
                'col-md-12 ' +
                styles.padd_remove +
                ' ' +
                styles.modifyOpsCollapsedtitle
              }
            >
              <i>Applicable only if update operation is selected.</i>
            </div>
          )}
        </div>
      </div>
    </div>
  );

  return (
    <div className={`${styles.container} ${styles.borderBottom}`}>
      <div className={styles.modifySection}>
        <Heading as="h4" fontSize="15px" mb="20px">
          Trigger Operations
          <ToolTip message="Edit operations and related columns" ml="sm" />
        </Heading>
        <Editor
          editorCollapsed={collapsed}
          editorExpanded={expanded}
          styles={styles}
          property="ops"
          ongoingRequest={modifyTrigger.ongoingRequest}
          service="modify-trigger"
          saveFunc={save}
          expandCallback={setValues}
        />
      </div>
    </div>
  );
};

export default OperationEditor;
