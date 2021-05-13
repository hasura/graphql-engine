import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';

const editorLabel = 'Handler';
const editorTooltip = 'The HTTP handler for the action';

const HandlerEditor = ({ value, onChange, className, disabled = false }) => {
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
        disabled={disabled}
        type="text"
        value={value}
        onChange={onChange}
        placeholder="http://custom-logic.com/api"
        className={`form-control ${styles.inputWidthLarge}`}
        data-test="action-create-handler-input"
      />
      <br />
      <small>
        Note: You can use an env var to template the handler URL if you have
        different URLs for multiple environments. e.g.{' '}
        <i>{'{{ACTION_BASE_URL}}/handler'}</i>
      </small>
    </div>
  );
};

export default HandlerEditor;
