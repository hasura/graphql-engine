import HealthStatus from './HealthStatus';
import styles from '../MetricsV1.module.scss';
import DBEvents from './DBEvents';
import { FaClone, FaDatabase, FaHome } from 'react-icons/fa';
import { PostgresSource } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';

const ReadReplicasGroup = ({
  source,
  isInconsistent,
}: {
  source: PostgresSource;
  isInconsistent: boolean;
}) => {
  const replicas =
    source?.configuration?.read_replicas?.map((_, i) => {
      return `Replica ${i}`;
    }) || [];

  return (
    <li>
      <div className={`${styles['dagContainer']}`}>
        <div className={styles['dagHeader']}>
          <Flex className="font-bold" align="center" gap="1">
            <FaClone aria-hidden="true" /> Read Replica Group
          </Flex>
        </div>

        <Flex className={styles['dagBodyGroup']} direction="column" gap="2">
          {[source.name, ...replicas].map((name, ix) => (
            <div
              key={name || `${source?.name}_Replica_${ix}`}
              className={`${styles['mb_xs']} ${styles['dagCard']} ${styles['dagCardClickable']}}`}
            >
              <div className={`${styles['dagHeader']} ${styles['flexMiddle']}`}>
                <Flex className="font-bold" gap="1" align="center">
                  <FaDatabase className={styles['mr_xxs']} aria-hidden="true" />
                  {ix === 0 ? (
                    <FaHome className={styles['mr_xxs']} aria-hidden="true" />
                  ) : (
                    <FaClone className={styles['mr_xxs']} aria-hidden="true" />
                  )}

                  {name}
                </Flex>
              </div>
              <div className={styles['dagBody']}>
                <HealthStatus
                  status={isInconsistent ? 'Inconsistent' : 'Healthy'}
                  showStatusText
                />
              </div>
            </div>
          ))}
        </Flex>
      </div>
      <DBEvents source={source} />
    </li>
  );
};

export default ReadReplicasGroup;
