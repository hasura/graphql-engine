import React from 'react';
import styles from './SelectOption.scss';

const SelectOption = props => {
  const { children, type, size } = props;

  return (
    <div className={styles.commonSelectBtn}>
      <select>
        <option>Select</option>
        <option>Select 1</option>
        <option>Select 2</option>
        <option>Select 3</option>
        <option>Select 4</option>
      </select>
    </div>
  );
};

export default SelectOption;
