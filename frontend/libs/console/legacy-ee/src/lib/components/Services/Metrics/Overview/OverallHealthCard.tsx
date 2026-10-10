import { useEffect, useState } from 'react';
import Endpoints from '../../../../Endpoints';
import HealthStatus from './HealthStatus';

import styles from '../MetricsV1.module.scss';
import { Flex } from '@radix-ui/themes';

const OverallHealthCard = () => {
  const url = Endpoints.health;
  const [healthzStatus, sethealthzStatus] = useState('');
  const [loading, setLoading] = useState(false);
  useEffect(() => {
    setLoading(true);
    fetch(url)
      .then((res) => res.text())
      .then((result) => {
        setLoading(false);

        if (result === 'OK') {
          sethealthzStatus('Healthy');
        } else if (result === 'WARN: inconsistent objects in schema') {
          sethealthzStatus('Inconsistent');
        }
      })
      .catch((error) => {
        setLoading(false);
        sethealthzStatus('Error');
        console.warn(error);
      });
  }, []);

  return (
    <div className={styles['dagCard']}>
      <div className={`${styles['dagHeader']} ${styles['flexMiddle']} `}>
        <p className="font-bold">GraphQL Engine</p>
      </div>
      <div className={styles.dagBody}>
        {loading ? (
          'Loading...'
        ) : (
          <Flex gap="2" align="center">
            <HealthStatus status={healthzStatus} />
            {healthzStatus}
          </Flex>
        )}
      </div>
    </div>
  );
};

export default OverallHealthCard;
