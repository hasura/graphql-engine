import React from 'react';
import { FaCheck } from 'react-icons/fa';
import styles from '../Common.module.scss';

const Check = ({ className = '', title = '' }) => {
  return (
    <FaCheck
      className={`${styles.iconCheck} ${className}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default Check;
