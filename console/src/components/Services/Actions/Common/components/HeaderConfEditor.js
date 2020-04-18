import React from 'react';

import Headers from '../../../../Common/Headers/Headers';
import { ToolTip, Heading, Box } from '../../../../UIKit/atoms';
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
      <Heading type="subHeading" mb="xs">
        {editorLabel}
        <ToolTip message={editorTooltip} ml="sm" />
      </Heading>
      <Box mb="20px">
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
      </Box>
      {getHeaders()}
    </div>
  );
};

export default HandlerEditor;
