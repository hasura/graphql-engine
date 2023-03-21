import React, { useEffect, useState } from 'react';
import Endpoints from '../../../../Endpoints';
import HealthStatus from './HealthStatus';

import styles from '../MetricsV1.module.scss';
import { FaSearch } from 'react-icons/fa';

const OverallHealthCard = () => {
  const url = Endpoints.health;
  const [healthzStatus, sethealthzStatus] = useState('');
  const [loading, setLoading] = useState(false);
  useEffect(() => {
    setLoading(true);
    fetch(url)
      .then(res => res.text())
      .then(result => {
        setLoading(false);

        if (result === 'OK') {
          sethealthzStatus('Healthy');
        } else if (result === 'WARN: inconsistent objects in schema') {
          sethealthzStatus('Inconsistent');
        }
      })
      .catch(error => {
        setLoading(false);
        sethealthzStatus('Error');
        console.warn(error);
      });
  }, []);
  return (
    <div className={`${styles.dagCard} ${styles.gql}`}>
      <div className={`${styles.dagHeader} ${styles.flexMiddle} `}>
        <p className={`${styles.strong} ${styles.mr_xs}`}>GraphQL Engine</p>
        <FaSearch
          className={`${styles.muted} ${styles.search}`}
          aria-hidden="true"
        />
      </div>
      <div className={styles.dagBody}>
        {loading ? (
          'Loading...'
        ) : (
          <p className={`${styles.sm} ${styles.muted} ${styles.no_pad}`}>
            <HealthStatus status={healthzStatus} />
            {healthzStatus}
          </p>
        )}
      </div>
    </div>
  );
};

export default OverallHealthCard;
