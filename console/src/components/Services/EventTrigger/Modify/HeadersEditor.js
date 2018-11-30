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
    });
  };

  render() {
    const { headers, styles, save, modifyTrigger, dispatch } = this.props;
    const collapsed = toggleButton => (
      <div className={styles.modifyOpsCollapsed}>
        {toggleButton('Edit')}
        <div className={styles.modifyHeaders}>
          {headers.length > 0 &&
            headers.map(h => {
              const { name, value, value_from_env } = h;
              return (
                <div className={styles.modifyHeadersCollapsedContent}>
                  <div className={styles.headersInputWrapper}>
                    <input
                      type="text"
                      className={`${styles.input} form-control ${
                        styles.add_mar_right
                      } ${styles.modifyHeaderCollapsedInput}`}
                      value={name}
                      disabled
                    />
                    <input
                      type="text"
                      className={`${styles.input} form-control ${
                        styles.add_mar_right
                      } ${styles.modifyHeaderCollapsedInput}`}
                      value={value || value_from_env}
                      disabled
                    />
                    {value_from_env && <p>(from env)</p>}
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
              <div className={styles.modifyHeadersCollapsedContent}>
                <input
                  type="text"
                  className={`${styles.input} form-control ${
                    styles.add_mar_right
                  } ${styles.modifyHeadersTextbox}`}
                  value={h.key}
                  onChange={e => {
                    dispatch(setHeaderKey(e.target.value, i));
                  }}
                  placeholder="key"
                />
                <select
                  value={h.type}
                  className={styles.modifyHeadersTextbox}
                  className={`${styles.select} ${
                    styles.selectWidth
                  } form-control ${styles.add_pad_left} ${
                    styles.add_mar_right
                  } ${styles.modifyHeadersTextbox}`}
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
                  className={`${styles.input} form-control ${
                    styles.add_mar_right
                  } ${styles.modifyHeadersTextbox}`}
                  value={h.value}
                  onChange={e => {
                    dispatch(setHeaderValue(e.target.value, i));
                  }}
                  placeholder={'value'}
                />
                <i
                  className={`${styles.fontAwosomeClose}
                      ${styles.removeHeader}
                      ${i !== modifyTrigger.headers.length - 1 &&
                        'fa-lg fa fa-times'}
                    `}
                  onClick={() => {
                    dispatch(removeHeader(i));
                  }}
                />
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
