import React from 'react';
import styles from './Styles.scss';

const ArgumentInput = ({ argument, setArgument, types }) => {
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

  const allTypes = [
    ...types.input_objects.map(t => t.name),
    ...types.scalars.map(t => t.name),
    ...types.enums.map(t => t.name),
    ...types.objects.map(t => t.name),
  ];

  const noTypes = allTypes.length === 0;
  const dropdownTitle = noTypes ? 'Create a type first' : null;

  return (
    <div className={`${styles.display_flex}`}>
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
        title={dropdownTitle}
        disabled={noTypes}
        onChange={typeOnChange}
      >
        {allTypes.map(t => {
          return (
            <option key={t} value={t}>
              {t}
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
          styles.add_mar_right_small
        }`}
      />
    </div>
  );
};

export default ArgumentInput;
