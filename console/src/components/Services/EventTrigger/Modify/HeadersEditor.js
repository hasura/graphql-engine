import React from 'react';
import Editor from './Editor';

import {
  addHeader,
  removeHeader,
  setHeaderKey,
  setHeaderType,
  setHeaderValue,
} from './Actions';

class HeadersEditor extends React.Component {
  componentDidUpdate() {
    const { dispatch, modifyTrigger } = this.props;
    const lastHeader = modifyTrigger.headers[modifyTrigger.headers.length - 1];
    if (lastHeader.key && lastHeader.value && lastHeader.type) {
      dispatch(addHeader());
    }
  }

  setValues = () => {
    const { dispatch, headers } = this.props;
    headers.forEach((h, i) => {
      const { name, value, value_from_env } = h;
      dispatch(setHeaderKey(name, i));
      dispatch(setHeaderType(value_from_env ? 'env' : 'static', i));
      dispatch(setHeaderValue(value || value_from_env, i));
      dispatch(addHeader());
    });
  };

  render() {
    const { headers, styles, save, modifyTrigger, dispatch } = this.props;
    const collapsed = toggleButton => (
      <div className={styles.modifyOpsCollapsed}>
        {toggleButton('Edit')}
        <div className={styles.modifyOps}>
          {headers.length > 0 &&
            headers.map(h => {
              const { name, value, value_from_env } = h;
              return (
                <div className={styles.modifyOpsCollapsedContent}>
                  <div className={styles.opsCheckboxWrapper}>
                    <input
                      type="text"
                      className={styles.modifyHeadersTextbox}
                      value={name}
                    />
                    <input
                      type="text"
                      className={styles.modifyHeadersTextbox}
                      value={value || value_from_env}
                    />
                    {value_from_env && '(from env var)'}
                  </div>
                </div>
              );
            })}
        </div>
        {headers.length === 0 && (
          <div className={styles.modifyProperty}>No headers</div>
        )}
      </div>
    );

    const expanded = (toggleButton, saveButton) => (
      <div className={styles.modifyOpsExpanded}>
        {toggleButton('Close')}
        <div className={styles.modifyOpsPadLeft}>
          {modifyTrigger.headers.map((h, i) => {
            return (
              <div className={styles.modifyOpsCollapsedContent}>
                <input
                  type="text"
                  className={styles.modifyHeadersTextbox}
                  value={h.key}
                  onChange={e => {
                    dispatch(setHeaderKey(e.target.value, i));
                  }}
                />
                <select
                  value={h.type}
                  className={styles.modifyHeadersTextbox}
                  onChange={e => {
                    dispatch(setHeaderType(e.target.value, i));
                  }}
                  data-test={`header-type-${i}`}
                >
                  <option value="static" key="0" title="static">
                    static
                  </option>
                  <option value="env" key="1" title="env">
                    from env variable
                  </option>
                </select>
                <input
                  type="text"
                  className={styles.modifyHeadersTextbox}
                  value={h.value}
                  onChange={e => {
                    dispatch(setHeaderValue(e.target.value, i));
                  }}
                />
                {i !== modifyTrigger.headers.length - 1 && (
                  <i
                    className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
                    onClick={() => {
                      dispatch(removeHeader(i));
                    }}
                  />
                )}
              </div>
            );
          })}
        </div>
        {saveButton(save)}{' '}
      </div>
    );

    return (
      <div className={styles.container}>
        <div className={styles.modifySection}>
          <h4 className={styles.modifySectionHeading}>Headers</h4>
          <Editor
            editorCollapsed={collapsed}
            editorExpanded={expanded}
            toggleCallback={this.setValues}
            styles={styles}
          />
        </div>
      </div>
    );
  }
}

export default HeadersEditor;
