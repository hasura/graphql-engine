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
        {toggleButton('Edit')}
        <div className={styles.modifyOps}>
          <div className={styles.modifyOpsCollapsedContent}>
            Operations:
            {queryTypes.map(qt => (
              <div className={styles.opsCheckboxWrapper}>
                <input
                  type="checkbox"
                  className={styles.opsCheckbox}
                  checked={Boolean(definition[qt])}
                />
                {qt}
              </div>
            ))}
          </div>
          <div className={styles.modifyOpsCollapsedContent}>
            Listen columns for update:&nbsp;
            {definition.update ? (
              allTableColumns.map(col => (
                <div className={styles.opsCheckboxWrapper}>
                  <input
                    type="checkbox"
                    className={styles.opsCheckbox}
                    checked={Boolean(
                      definition.update.columns.find(c => c === col)
                    )}
                  />
                  {col}
                </div>
              ))
            ) : (
              <i>Applicable only for update operation</i>
            )}
          </div>
        </div>
      </div>
    );

    const expanded = (toggleButton, saveButton) => (
      <div className={styles.modifyOpsExpanded}>
        {toggleButton('Close')}
        <div className={styles.modifyOpsPadLeft}>
          <div className={styles.modifyOpsCollapsedContent}>
            Operations:
            {queryTypes.map(qt => (
              <div className={styles.opsCheckboxWrapper}>
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
          <div className={styles.modifyOpsCollapsedContent}>
            Listen columns for update:&nbsp;
            {modifyTrigger.definition.update ? (
              allTableColumns.map(col => (
                <div className={styles.opsCheckboxWrapper}>
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
              <i>Applicable only for update operation</i>
            )}
          </div>
        </div>
        {saveButton(save)}
      </div>
    );

    return (
      <div className={styles.container}>
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
