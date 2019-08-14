import React from 'react';
import styles from '../Common.scss';

const Cross = ({ className }) => {
  return (
    <i
      className={`fa fa-times ${styles.iconCross} ${className}`}
      aria-hidden="true"
    />
  );
};

export default Cross;
