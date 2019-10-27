import React from 'react';
import styles from './Styles.scss';
import RemoveIcon from '../../../../Common/Icons/Remove';

const ArgumentEditor = ({
  argument,
  setArgument,
  removeArgument,
  allTypes,
  isLast,
}) => {
  const { name, type, description, optional } = argument;

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

  const toggleOptional = e => {
    setArgument({
      ...argument,
      optional: e.target.checked,
    });
  };

  const noTypes = allTypes.length === 0;

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
      <input
        type="text"
        value={description}
        onChange={descriptionOnChange}
        placeholder="description"
        className={`form-control ${styles.inputWidth} ${
          styles.add_mar_right_mid
        }`}
      />
      <label className={`${styles.add_mar_right_mid} ${styles.cursorPointer}`}>
        <input
          type="checkbox"
          checked={optional}
          onChange={toggleOptional}
          className={`${styles.add_mar_right_small} ${styles.cursorPointer}`}
        />
        Optional
      </label>
      {removeIcon}
    </div>
  );
};

export default ArgumentEditor;
