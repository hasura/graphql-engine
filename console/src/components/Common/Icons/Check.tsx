import React from 'react';
import styles from '../Common.scss';

const Check = ({ className = '', title = '' }) => {
  return (
    <i
      className={`fa fa-check ${styles.iconCheck} ${className}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Check;
