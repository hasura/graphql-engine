import React from 'react';
import Editor from '../../../Common/Layout/ExpandableEditor/Editor';
import Tooltip from './Tooltip';

import { toggleQueryType, toggleColumn, toggleManualType } from './Actions';

import {
  getValidQueryTypes,
  queryToInternalNameMap,
  INTERNAL_CONSOLE_QUERY_REP,
} from './utils';

const toggleOperation = upObj => {
  if (upObj.query === INTERNAL_CONSOLE_QUERY_REP) {
    return toggleManualType(upObj);
  }
  return toggleQueryType(upObj);
};

class OperationEditor extends React.Component {
  setValues = () => {
    const { dispatch, definition } = this.props;
    /*
     * Loop through the keys in definition,
     * this object will have actual internal name.
     * No need to transform from display to internal name
     * */
    for (const queryType in definition) {
      if (definition[queryType]) {
        if (queryType !== INTERNAL_CONSOLE_QUERY_REP) {
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
              value: definition[queryType] || false,
            })
          );
        }
      }
    }
  };

  render() {
    const {
      definition,
      allTableColumns,
      styles,
      save,
      modifyTrigger,
      dispatch,
    } = this.props;
    const queryTypes = getValidQueryTypes();
    const collapsed = () => (
      <div className={styles.modifyOps}>
        <div className={styles.modifyOpsCollapsedContent}>
          <div className={'col-md-12 ' + styles.padd_remove}>Operations:</div>
          <div className={'col-md-12 ' + styles.padd_remove}>
            {queryTypes.map((qt, i) => (
              <div
                className={
                  styles.opsCheckboxWrapper + ' col-md-4 ' + styles.padd_remove
                }
                key={i}
              >
                <input
                  type="checkbox"
                  className={styles.opsCheckboxDisabled}
                  checked={Boolean(definition[queryToInternalNameMap[qt]])}
                  disabled
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
            {definition.update ? (
              allTableColumns.map((col, i) => (
                <div
                  className={`${styles.opsCheckboxWrapper} col-md-4 ${
                    styles.padd_remove
                  }`}
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
                <i>(Applicable only for update operation)</i>
              </div>
            )}
          </div>
        </div>
      </div>
    );

    const expanded = () => (
      <div className={styles.modifyOpsPadLeft}>
        <div className={styles.modifyOpsCollapsedContent}>
          <div className={'col-md-12 ' + styles.padd_remove}>Operations:</div>
          <div className={'col-md-12 ' + styles.padd_remove}>
            {queryTypes.map((qt, i) => (
              <div
                className={`${styles.opsCheckboxWrapper} col-md-4 ${
                  styles.padd_remove
                } ${styles.cursorPointer}`}
                key={i}
                onClick={() => {
                  dispatch(
                    toggleOperation({
                      query: queryToInternalNameMap[qt],
                      columns: allTableColumns.map(c => c.name),
                      value: !modifyTrigger.definition[
                        queryToInternalNameMap[qt]
                      ],
                    })
                  );
                }}
              >
                <input
                  type="checkbox"
                  className={`${styles.opsCheckbox} ${styles.cursorPointer}`}
                  checked={Boolean(
                    modifyTrigger.definition[queryToInternalNameMap[qt]]
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
                  className={`${styles.opsCheckboxWrapper} col-md-4 ${
                    styles.padd_remove
                  } ${styles.cursorPointer}`}
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
                <i>(Applicable only for update operation)</i>
              </div>
            )}
          </div>
        </div>
      </div>
    );

    return (
      <div className={`${styles.container} ${styles.borderBottom}`}>
        <div className={styles.modifySection}>
          <h4 className={styles.modifySectionHeading}>
            Operations <Tooltip message="Edit operations and related columns" />
          </h4>
          <Editor
            editorCollapsed={collapsed}
            editorExpanded={expanded}
            styles={styles}
            property="ops"
            ongoingRequest={modifyTrigger.ongoingRequest}
            service="modify-trigger"
            saveFunc={save}
            expandCallback={this.setValues}
          />
        </div>
      </div>
    );
  }
}

export default OperationEditor;
