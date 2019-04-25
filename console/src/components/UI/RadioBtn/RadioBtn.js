import React from 'react';
import styles from './RadioBtn.scss';

const RadioBtn = props => {
  const { children } = props;

  return (
    <span className={styles.radio}>
      <input type="radio" />
      <label>{children}</label>
    </span>
  );
};

export default RadioBtn;
