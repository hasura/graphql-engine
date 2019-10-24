import React from 'react';
import styles from './Styles.scss';

const ArgumentEditor = ({
  argument,
  setArgument,
  removeArgument,
  allTypes,
  isLast,
}) => {
  const { name, type, description } = argument;

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

  const noTypes = allTypes.length === 0;

  // show remove icon for all columns except last
  let removeIcon = null;
  if (!isLast) {
    removeIcon = (
      <i
        className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
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
      {removeIcon}
    </div>
  );
};

export default ArgumentEditor;
