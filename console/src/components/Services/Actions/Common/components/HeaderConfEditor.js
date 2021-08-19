import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';
import Headers from '../../../../Common/Headers/Headers';

const editorLabel = 'Headers';
const editorTooltip =
  'Headers Hasura will send to the webhook with the POST request';

const HeaderConfEditor = ({
  className,
  forwardClientHeaders,
  toggleForwardClientHeaders,
  headers,
  setHeaders,
  disabled = false,
}) => {
  return (
    <div className={`${className || ''}`}>
      <h2
        className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
      >
        {editorLabel}
        <Tooltip
          id="action-name"
          text={editorTooltip}
          className={styles.add_mar_left_mid}
        />
      </h2>
      <div className={`${styles.add_mar_bottom_mid}`}>
        <label className={`${styles.add_mar_right} ${styles.cursorPointer}`}>
          <input
            type="checkbox"
            checked={forwardClientHeaders}
            onChange={toggleForwardClientHeaders}
            className={`${styles.add_mar_right_small} ${styles.cursorPointer} legacy-input-fix`}
            disabled={disabled}
          />
          Forward client headers to webhook
        </label>
      </div>
      <Headers headers={headers} setHeaders={setHeaders} disabled={disabled} />
    </div>
  );
};

export default HeaderConfEditor;
