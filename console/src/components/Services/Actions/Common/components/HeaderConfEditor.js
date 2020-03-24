import React from 'react';

import Headers from '../../../../Common/Headers/Headers';
import { ToolTip } from '../../../../UIKit/atoms';
import styles from './Styles.scss';

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
        <ToolTip
          message={editorTooltip}
          className={styles.add_mar_left_mid}
          ml="md"
        >
          {editorLabel}
        </ToolTip>
      </h2>
      <div className={`${styles.add_mar_bottom_mid}`}>
        <label
          className={`${styles.add_mar_right} ${styles.cursorPointer}`}
          onClick={toggleForwardClientHeaders}
        >
          <input
            type="checkbox"
            checked={forwardClientHeaders}
            readOnly
            className={`${styles.add_mar_right_small}`}
          />
          Forward client headers to webhook
        </label>
      </div>
      {getHeaders()}
    </div>
  );
};

export default HandlerEditor;
