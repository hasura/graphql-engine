import React from 'react';
import { FaExclamationTriangle } from 'react-icons/fa';
import styles from '../Common.module.scss';

const ExclamationTriangle = ({ className = '', title = '' }) => {
  return (
    <FaExclamationTriangle
      className={`${styles.iconExclamationTriangle} ${className}`}
      aria-hidden="true"
      title={title}
    />
  );
};

export default ExclamationTriangle;
