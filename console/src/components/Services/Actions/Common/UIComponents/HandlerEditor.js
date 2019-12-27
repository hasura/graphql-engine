import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';

const editorLabel = 'Handler';
const editorTooltip =
  'Set a handler. This handler will be called with mutation payload';

const HandlerEditor = ({ value, onChange, className, placeholder }) => {
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
      <input
        type="text"
        value={value}
        onChange={onChange}
        placeholder={placeholder}
        className={`form-control ${styles.inputWidth}`}
      />
    </div>
  );
};

export default HandlerEditor;
