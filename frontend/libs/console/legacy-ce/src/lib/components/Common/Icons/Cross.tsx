import React from 'react';
import { FaTimes } from 'react-icons/fa';

import styles from '../Common.module.scss';

const Cross = ({ className = '', title = '' }) => {
  return (
    <FaTimes
      className={` ${styles.iconCross} ${className}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Cross;
