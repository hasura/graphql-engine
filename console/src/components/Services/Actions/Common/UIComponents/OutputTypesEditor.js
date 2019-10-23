import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';

const editorLabel = 'Output Type';
const editorTooltip = 'This is the returning type of the mutation';

const OutputTypesEditor = ({ className, value, onChange }) => {
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
      <select
        className={`form-control ${styles.inputWidth}`}
        value={value || ''}
        onChange={onChange}
      >
        {!value && <option>--output type--</option>}
        <option>Type1</option>
        <option>Type2</option>
      </select>
    </div>
  );
};

export default OutputTypesEditor;
