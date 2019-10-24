import React from 'react';
import styles from './Styles.scss';

const FieldEditor = ({ field, setField, allTypes, removeField, isLast }) => {
  const { name, type } = field;

  const nameOnChange = e => {
    setField({
      ...field,
      name: e.target.value,
    });
  };

  const typeOnChange = e => {
    setField({
      ...field,
      type: e.target.value,
    });
  };

  const noTypes = allTypes.length === 0;

  // show remove icon for all columns except last
  let removeIcon = null;
  if (!isLast) {
    removeIcon = (
      <i
        className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
        onClick={removeField}
      />
    );
  }

  return (
    <div className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}>
      <input
        type="text"
        value={name}
        onChange={nameOnChange}
        placeholder="field name"
        className={`form-control ${styles.inputWidth} ${
          styles.add_mar_right_small
        }`}
      />
      <select
        className={`form-control ${styles.inputWidthMid} ${
          styles.add_mar_right_small
        }`}
        value={type || ''}
        disabled={noTypes}
        onChange={typeOnChange}
      >
        {!type && (
          <option key="" value="">
            {' '}
            -- type --{' '}
          </option>
        )}
        {allTypes.map(t => {
          return (
            <option key={t} value={t}>
              {t}
            </option>
          );
        })}
      </select>
      {removeIcon}
    </div>
  );
};

export default FieldEditor;
