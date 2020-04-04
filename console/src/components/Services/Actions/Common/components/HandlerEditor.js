import React from 'react';

import { ToolTip } from '../../../../UIKit/atoms';
import styles from './Styles.scss';

const editorLabel = 'Handler';
const editorTooltipText = 'The HTTP handler for the action';

const HandlerEditor = ({ value, onChange, className }) => {
  return (
    <div className={className || ''}>
      <h2
        className={`${styles.subheading_text} ${styles.add_mar_bottom_small}`}
      >
        {editorLabel}
        <ToolTip message={editorTooltipText} ml="sm" />
      </h2>
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
