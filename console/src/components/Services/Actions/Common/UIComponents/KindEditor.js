import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';

const editorLabel = 'Kind';
const editorTooltip = 'Synchronous or Asynchronous';
// Tooltip todo

const HandlerEditor = ({ value, onChange, className }) => {
  const setAsynchronous = () => {
    onChange('asynchronous');
  };

  const setSynchronous = () => {
    onChange('synchronous');
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
      <div className={styles.display_flex}>
        <label
          className={`${styles.add_mar_right} ${styles.cursorPointer}`}
          onClick={setSynchronous}
        >
          <input
            type="radio"
            checked={value === 'synchronous'}
            className={`${styles.add_mar_right_small}`}
          />
          Synchronous
        </label>
        <label
          onClick={setAsynchronous}
          className={`${styles.add_mar_right} ${styles.cursorPointer}`}
        >
          <input
            type="radio"
            checked={value === 'asynchronous'}
            className={`${styles.add_mar_right_small}`}
          />
          Asynchronous
        </label>
      </div>
    </div>
  );
};

export default HandlerEditor;
