import React from 'react';

import styles from './LoadingSpinner.scss';

const LoadingSpinner = () => {
  return (
    <div className={styles['lds-spinner']}>
      <div />
      <div />
      <div />
      <div />
      <div />
      <div />
      <div />
      <div />
    </div>
  );
};

export default LoadingSpinner;
