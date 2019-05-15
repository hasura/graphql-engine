import React from 'react';
import styles from '../Common.scss';

const Check = ({ className }) => {
  return (
    <i
      className={`fa fa-check ${styles.iconCheck} ${className}`}
      aria-hidden="true"
    />
  );
};

export default Check;
