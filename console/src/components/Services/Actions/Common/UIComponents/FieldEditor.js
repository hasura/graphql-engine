import React from 'react';
import styles from './Styles.scss';

const FieldEditor = ({ field, setField, allTypes }) => {
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
    </div>
  );
};

export default FieldEditor;
