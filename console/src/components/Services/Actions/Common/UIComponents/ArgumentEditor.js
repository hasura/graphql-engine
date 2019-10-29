import React from 'react';
import styles from './Styles.scss';
import RemoveIcon from '../../../../Common/Icons/Remove';
import { typeWrappers } from '../stateDefaults';

const ArgumentEditor = ({
  argument,
  setArgument,
  removeArgument,
  allTypes,
  isLast,
  parentTypeKind,
}) => {
  const { name, type, description, typeWrap } = argument;

  const nameOnChange = e => {
    setArgument({
      ...argument,
      name: e.target.value,
    });
  };
  const typeOnChange = e => {
    setArgument({
      ...argument,
      type: e.target.value,
    });
  };
  const descriptionOnChange = e => {
    setArgument({
      ...argument,
      description: e.target.value,
    });
  };

  const settypeWrappers = e => {
    setArgument({
      ...argument,
      typeWrap: e.target.value,
    });
  };

  const noTypes = allTypes.length === 0;

  const typeWrapTitle = noTypes ? 'Select a type first' : null;

  // show remove icon for all columns except last
  let removeIcon = null;
  if (!isLast) {
    removeIcon = (
      <RemoveIcon
        className={`${styles.cursorPointer}`}
        onClick={removeArgument}
      />
    );
  }

  return (
    <div className={`${styles.display_flex} ${styles.add_mar_bottom_mid}`}>
      <input
        type="text"
        value={name}
        onChange={nameOnChange}
        placeholder="argument name"
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
          if (t.kind === 'object') return null;
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
        onChange={settypeWrappers}
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

export default ArgumentEditor;
