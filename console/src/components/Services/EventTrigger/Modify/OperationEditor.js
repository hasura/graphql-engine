import React from 'react';
import Editor from './Editor';

import { toggleQueryType, toggleColumn } from './Actions';

class OperationEditor extends React.Component {
  setValues = () => {
    const { dispatch, definition } = this.props;
    for (const queryType in definition) {
      if (definition[queryType]) {
        dispatch(toggleQueryType(queryType, definition[queryType].columns));
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
    const queryTypes = ['delete', 'update', 'insert'];
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
                    className={
                      styles.opsCheckboxWrapper + ' col-md-4 ' + styles.noPadd
                    }
                    key={i}
                  >
                    <input
                      type="checkbox"
                      className={styles.opsCheckboxDisabled}
                      checked={Boolean(
                        definition.update.columns.find(c => c === col)
                      )}
                      disabled
                    />
                    {col}
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
                  className={
                    styles.opsCheckboxWrapper + ' col-md-4 ' + styles.noPadd
                  }
                  key={i}
                >
                  <input
                    type="checkbox"
                    className={styles.opsCheckbox}
                    checked={Boolean(modifyTrigger.definition[qt])}
                    onChange={() => {
                      dispatch(toggleQueryType(qt, allTableColumns));
                    }}
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
                    className={
                      styles.opsCheckboxWrapper + ' col-md-4 ' + styles.noPadd
                    }
                    key={i}
                  >
                    <input
                      type="checkbox"
                      className={styles.opsCheckbox}
                      checked={Boolean(
                        modifyTrigger.definition.update.columns.find(
                          c => c === col
                        )
                      )}
                      onChange={() => {
                        dispatch(toggleColumn('update', col));
                      }}
                    />
                    {col}
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
          <h4 className={styles.modifySectionHeading}>Operations</h4>
          <Editor
            editorCollapsed={collapsed}
            editorExpanded={expanded}
            styles={styles}
            toggleCallback={this.setValues}
          />
        </div>
      </div>
    );
  }
}

export default OperationEditor;
