import React from 'react';
import styles from './RadioBtn.scss';

const RadioBtn = props => {
  const { children } = props;

  return (
    <div className={styles.commonRadioBtn}>
      <input type="radio" id={children} name="radio-group" checked />
      <label htmlFor={children}>{children}</label>
    </div>
  );
};

export default RadioBtn;
