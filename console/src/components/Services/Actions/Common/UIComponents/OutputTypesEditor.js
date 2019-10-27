import React from 'react';
import styles from './Styles.scss';
import Tooltip from './Tooltip';

const editorLabel = 'Output Type';
const editorTooltip = 'This is the returning type of the mutation';

const OutputTypesEditor = ({ className, value, onChange, allTypes }) => {
  const dropdownDisabled = !allTypes.length;
  const dropdownTitle = dropdownDisabled ? 'Create a custom type first' : null;
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
        disabled={dropdownDisabled}
        title={dropdownTitle}
      >
        {!value && <option>--type--</option>}
        {allTypes.map((t, i) => {
          if (t.kind !== 'object') return null;
          return (
            <option key={i} value={i}>
              {t.name}
            </option>
          );
        })}
      </select>
    </div>
  );
};

export default OutputTypesEditor;
