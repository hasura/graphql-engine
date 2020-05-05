import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';
import Headers from '../../../../Common/Headers/Headers';

const editorLabel = 'Headers';
const editorTooltip =
  'Headers Hasura will send to the webhook with the POST request';
// Tooltip todo

const HandlerEditor = ({
  className,
  forwardClientHeaders,
  toggleForwardClientHeaders,
  headers,
  setHeaders,
}) => {
  const getHeaders = () => {
    return <Headers headers={headers} setHeaders={setHeaders} />;
  };

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
        <label className={styles.add_mar_right}>
          <input
            type="checkbox"
            checked={forwardClientHeaders}
            onChange={toggleForwardClientHeaders}
            className={`${styles.add_mar_right_small} ${styles.cursorPointer}`}
          />
          Forward client headers to webhook
        </label>
      </div>
      {getHeaders()}
    </div>
  );
};

export default HandlerEditor;
