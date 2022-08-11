import * as React from 'react';
import { FaCheck, FaCircleNotch, FaTimes } from 'react-icons/fa';

import { ProgressState } from './types';
import styles from '../styles.module.scss';

const Pending: React.FC = () => {
  return (
    <div className={styles.progressCircle}>
      <FaCheck aria-hidden="true" />
    </div>
  );
};

const Complete: React.FC = () => {
  return (
    <div className={`${styles.progressCircle} ${styles.progressComplete}`}>
      <FaCheck aria-hidden="true" />
    </div>
  );
};

const InProgress: React.FC = () => {
  return <FaCircleNotch className="animate-spin text-2xl" aria-hidden="true" />;
};

const Error: React.FC = () => {
  return (
    <div className={`${styles.progressCircle} ${styles.progressError}`}>
      <FaTimes aria-hidden="true" />
    </div>
  );
};

const StatusIcon: React.FC<{
  state:
    | ProgressState['creating-app']
    | ProgressState['installing-postgres']
    | ProgressState['getting-config'];
}> = ({ state }) => {
  switch (state.status) {
    case 'pending':
      return <Pending />;
    case 'in-progress':
      return <InProgress />;
    case 'success':
      return <Complete />;
    case 'failed':
      return <Error />;
    default:
      return <Pending />;
  }
};

export default StatusIcon;
