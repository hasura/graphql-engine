import React from 'react';
import styles from '../Common.scss';

const Cross = ({ className = '', title = '' }) => {
  return (
    <i
      className={`fa fa-times ${styles.iconCross} ${className}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Cross;
