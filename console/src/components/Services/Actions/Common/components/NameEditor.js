import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';

const editorLabel = 'Name';
const editorTooltip =
  'Set a name for your action. This will be a root field in your GraphQL schema';

const NameEditor = ({ value, onChange, className, placeholder }) => {
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
        placeholder={placeholder}
        className={`form-control ${styles.inputWidth}`}
      />
    </div>
  );
};

export default NameEditor;
