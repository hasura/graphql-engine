import React from 'react';

import { ToolTip, Heading } from '../../../../UIKit/atoms';
import styles from './Styles.scss';

const editorLabel = 'Handler';
const editorTooltipText = 'The HTTP handler for the action';

const HandlerEditor = ({ value, onChange, className }) => {
  return (
    <div className={className || ''}>
      <Heading as="h2" fontSize="15px" mt="0px" mb="xs">
        {editorLabel}
        <ToolTip message={editorTooltipText} ml="sm" />
      </Heading>
      <input
        type="text"
        value={value}
        onChange={onChange}
        placeholder={'http://custom-logic.com/api'}
        className={`form-control ${styles.inputWidthLarge}`}
      />
    </div>
  );
};

export default HandlerEditor;
