import React from 'react';
import styles from './CheckBox.scss';

const CheckBox = props => {
  const { children } = props;

  return (
    <div className={styles.commonCheckBox}>
      <input id={children} type="checkbox" value="value1" />
      <label for={children}>{children}</label>
    </div>
  );
};

export default CheckBox;
