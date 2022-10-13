import React, { useState } from 'react';
import AceEditor from 'react-ace';
import { Button } from '@hasura/console-oss';
import { Tooltip } from '@hasura/console-oss';

import styles from '../Metrics.module.scss';

/** Workaround for https://github.com/ajaxorg/ace/issues/3320 */
window.ace.require = window.ace.acequire;
/* eslint-disable import/first */
import 'ace-builds/src-noconflict/mode-json';
import 'ace-builds/src-noconflict/theme-github';
/* eslint-enable import/first */

export const EditData = ({ label, data, onSave, onCancel }) => {
  const [value, setValue] = useState(data);
  const [validJSON, setValidJSON] = useState(true);

  const handleChange = (v) => {
    setValue(v);
    try {
      JSON.parse(v);
      setValidJSON(true);
    } catch (err) {
      setValidJSON(false);
    }
  };

  const handleSave = () => {
    if (validJSON) {
      onSave(value);
    }
  };

  return (
    <React.Fragment>
      <div className={styles.infoWrapper}>
        <div className={styles.information}>{label}: </div>
      </div>
      <div className={styles.boxwrapper + ' ' + styles.errorBox}>
        <div className={styles.box}>
          <AceEditor
            mode="json"
            theme="github"
            fontSize={14}
            name="blah2"
            onChange={handleChange}
            showPrintMargin={false}
            showGutter={false}
            highlightActiveLine
            value={value}
            className={styles.aceEditor}
            setOptions={{
              tabSize: 2,
            }}
            height="300px"
            width="unset"
          />
        </div>
        <div style={{ display: 'flex' }}>
          <Button
            className={`${styles.placementRight} ${styles.mrTop10} `}
            onClick={onCancel}
          >
            Cancel
          </Button>
          <SaveButtonOverlay show={!validJSON}>
            <Button
              mode="primary"
              disabled={!validJSON}
              style={{ marginLeft: 10, marginTop: 10 }}
              onClick={handleSave}
            >
              Save
            </Button>
          </SaveButtonOverlay>
        </div>
      </div>
    </React.Fragment>
  );
};

const SaveButtonOverlay = ({ show, children }) => {
  if (show) {
    return (
      <Tooltip
        side="top"
        tooltipContentChildren="Can't save. Not a valid JSON."
      >
        {children}
      </Tooltip>
    );
  }
  return children;
};
