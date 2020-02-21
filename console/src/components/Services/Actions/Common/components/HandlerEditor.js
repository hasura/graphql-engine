import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';

const editorLabel = 'Handler';
const editorTooltip = 'The HTTP handler for the action';

const HandlerEditor = ({ value, onChange, className }) => {
  return (
    <div className={className || ''}>
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
