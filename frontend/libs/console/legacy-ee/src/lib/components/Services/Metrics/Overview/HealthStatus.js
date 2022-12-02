import React from 'react';
import { FaCheckCircle, FaExclamationTriangle, FaInfo } from 'react-icons/fa';

import styles from '../MetricsV1.module.scss';

const getHealthStyle = status => {
  switch (status) {
    case 'Healthy':
      return <FaCheckCircle className={styles.green} aria-hidden="true" />;

    case 'UnHealthy':
      return (
        <FaExclamationTriangle className={styles.yellow} aria-hidden="true" />
      );

    case 'Inconsistent':
      return (
        <FaExclamationTriangle className={styles.yellow} aria-hidden="true" />
      );
    case 'Error':
      return (
        <FaExclamationTriangle className={styles.red} aria-hidden="true" />
      );
    default:
      return <FaInfo className={styles.blue} aria-hidden="true" />;
  }
};

const HealthStatus = ({ status = '', showStatusText = false }) => {
  return (
    <>
      {getHealthStyle(status)}
      {showStatusText ? ` ${status}` : ' '}
    </>
  );
};

export default HealthStatus;
