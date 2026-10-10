import { FaCheckCircle, FaExclamationTriangle, FaInfo } from 'react-icons/fa';

import styles from '../MetricsV1.module.scss';
import { Flex } from '@radix-ui/themes';

const getHealthStyle = (status) => {
  switch (status) {
    case 'Healthy':
      return <FaCheckCircle className={styles['green']} aria-hidden="true" />;

    case 'UnHealthy':
      return (
        <FaExclamationTriangle
          className={styles['yellow']}
          aria-hidden="true"
        />
      );

    case 'Inconsistent':
      return (
        <FaExclamationTriangle
          className={styles['yellow']}
          aria-hidden="true"
        />
      );
    case 'Error':
      return (
        <FaExclamationTriangle className={styles['red']} aria-hidden="true" />
      );
    default:
      return <FaInfo className={styles['blue']} aria-hidden="true" />;
  }
};

const HealthStatus = ({ status = '', showStatusText = false }) => {
  return (
    <Flex align="center" gap="1">
      {getHealthStyle(status)}
      {showStatusText ? ` ${status}` : ' '}
    </Flex>
  );
};

export default HealthStatus;
