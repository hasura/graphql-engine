import React from 'react';
import styles from './Styles.scss';
import RemoveIcon from '../../../../Common/Icons/Remove';

const EnumValueInput = ({ value, setValue, removeValue, isLast }) => {
  const valueNameOnChange = e =>
    setValue({
      ...value,
      value: e.target.value,
    });
  const valueDescriptionOnChange = e =>
    setValue({
      ...value,
      description: e.target.value,
    });

  console.log(isLast);

  const removeIcon = !isLast && (
    <RemoveIcon onClick={removeValue} className={styles.cursorPointer} />
  );

  return (
    <div className={styles.display_flex}>
      <input
        type="text"
        value={value.value}
        onChange={valueNameOnChange}
        placeholder="value"
        className={`form-control ${styles.add_mar_right_small} ${
          styles.inputWidth
        }`}
      />
      <input
        type="text"
        value={value.description}
        onChange={valueDescriptionOnChange}
        placeholder="description"
        className={`form-control ${styles.add_mar_right_small} ${
          styles.inputWidth
        }`}
      />
      {removeIcon}
    </div>
  );
};

export default EnumValueInput;
