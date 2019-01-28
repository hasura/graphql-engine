import React from 'react';
import Editor from './Editor';
import Tooltip from './Tooltip';

import { toggleQueryType, toggleColumn } from './Actions';

class OperationEditor extends React.Component {
  setValues = () => {
    const { dispatch, definition } = this.props;
    for (const queryType in definition) {
      if (definition[queryType]) {
        dispatch(
          toggleQueryType(queryType, definition[queryType].columns, true)
        );
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
    const queryTypes = ['insert', 'update', 'delete'];
    const collapsed = toggleButton => (
      <div className={styles.modifyOpsCollapsed}>
        <div>{toggleButton('Edit')}</div>
        <div className={styles.modifyOps}>
          <div className={styles.modifyOpsCollapsedContent}>
            <div className={'col-md-12 ' + styles.noPadd}>Operations:</div>
            <div className={'col-md-12 ' + styles.noPadd}>
              {queryTypes.map((qt, i) => (
                <div
                  className={
                    styles.opsCheckboxWrapper + ' col-md-4 ' + styles.noPadd
                  }
                  key={i}
                >
                  <input
                    type="checkbox"
                    className={styles.opsCheckboxDisabled}
                    checked={Boolean(definition[qt])}
                    disabled
                  />
                  {qt}
                </div>
              ))}
            </div>
          </div>
          <div className={styles.modifyOpsCollapsedContent}>
            <div className={'col-md-12 ' + styles.noPadd}>
              Listen columns for update:&nbsp;
            </div>
            <div className={'col-md-12 ' + styles.noPadd}>
              {definition.update ? (
                allTableColumns.map((col, i) => (
                  <div
                    className={`${styles.opsCheckboxWrapper} col-md-4 ${
                      styles.noPadd
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
                    styles.noPadd +
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
      </div>
    );

    const expanded = (toggleButton, saveButton) => (
      <div className={styles.modifyOpsExpanded}>
        <div>{toggleButton('Close')}</div>
        <div className={styles.modifyOpsPadLeft}>
          <div className={styles.modifyOpsCollapsedContent}>
            <div className={'col-md-12 ' + styles.noPadd}>Operations:</div>
            <div className={'col-md-12 ' + styles.noPadd}>
              {queryTypes.map((qt, i) => (
                <div
                  className={`${styles.opsCheckboxWrapper} col-md-4 ${
                    styles.noPadd
                  } ${styles.cursorPointer}`}
                  key={i}
                  onClick={() => {
                    dispatch(
                      toggleQueryType(
                        qt,
                        allTableColumns.map(c => c.name),
                        !modifyTrigger.definition[qt]
                      )
                    );
                  }}
                >
                  <input
                    type="checkbox"
                    className={`${styles.opsCheckbox} ${styles.cursorPointer}`}
                    checked={Boolean(modifyTrigger.definition[qt])}
                  />
                  {qt}
                </div>
              ))}
            </div>
          </div>
          <div className={styles.modifyOpsCollapsedContent}>
            <div className={'col-md-12 ' + styles.noPadd}>
              Listen columns for update:&nbsp;
            </div>
            <div className={'col-md-12 ' + styles.noPadd}>
              {modifyTrigger.definition.update ? (
                allTableColumns.map((col, i) => (
                  <div
                    className={`${styles.opsCheckboxWrapper} col-md-4 ${
                      styles.noPadd
                    } ${styles.cursorPointer}`}
                    key={i}
                    onClick={() => dispatch(toggleColumn('update', col.name))}
                  >
                    <input
                      type="checkbox"
                      className={`${styles.opsCheckbox} ${
                        styles.cursorPointer
                      }`}
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
                    styles.noPadd +
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
        {saveButton(save)}
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
            toggleCallback={this.setValues}
          />
        </div>
      </div>
    );
  }
}

export default OperationEditor;
