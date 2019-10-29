import React from 'react';
import styles from './Styles.scss';
import RemoveIcon from '../../../../Common/Icons/Remove';
import { typeWrappers } from '../stateDefaults';

const FieldEditor = ({
  field,
  setField,
  allTypes,
  removeField,
  isLast,
  parentTypeKind,
}) => {
  const { name, type, typeWrap } = field;

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

  const typeWrapperOnChange = e => {
    setField({
      ...field,
      typeWrap: e.target.value,
    });
  };

  const noTypes = allTypes.length === 0;

  // show remove icon for all columns except last
  let removeIcon = null;
  if (!isLast) {
    removeIcon = (
      <RemoveIcon className={`${styles.cursorPointer}`} onClick={removeField} />
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
        {allTypes.map((t, i) => {
          if (
            (parentTypeKind === 'object' && t.kind === 'input_object') ||
            (parentTypeKind === 'input_object' && t.kind === 'object')
          ) {
            return null;
          }
          return (
            <option key={i} value={i}>
              {t.name}
            </option>
          );
        })}
      </select>
      <select
        className={`form-control ${styles.inputWidthMid} ${
          styles.add_mar_right_small
        }`}
        value={typeWrap || ''}
        title={'Type wrapper'}
        onChange={typeWrapperOnChange}
      >
        {!typeWrap && (
          <option key="" value="">
            {' '}
            -- type --{' '}
          </option>
        )}
        {typeWrappers.map((w, i) => {
          return (
            <option key={i} value={i}>
              {w.label}
            </option>
          );
        })}
      </select>
      {removeIcon}
    </div>
  );
};

export default FieldEditor;
