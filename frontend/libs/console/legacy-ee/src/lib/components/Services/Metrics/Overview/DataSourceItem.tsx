import DBEvents from './DBEvents';
import HealthStatus from './HealthStatus';
import ReadReplicasGroup from './ReadReplicasGroup';

import styles from '../MetricsV1.module.scss';
import { FaDatabase } from 'react-icons/fa';
import { InconsistentObject, Source } from '@hasura/shared/types';
import { Flex } from '@radix-ui/themes';

const DatasourceItem = ({
  source,
  inconsistentObjects = [],
}: {
  source: Source;
  inconsistentObjects: InconsistentObject[];
}) => {
  const sourceName = source.name || '';
  const isInconsistent =
    inconsistentObjects.find(
      (i) => 'type' in i && i.type === 'source' && i.definition === sourceName,
    ) !== undefined;

  const hasReadReplicas =
    source.kind === 'postgres' &&
    Boolean(source?.configuration?.read_replicas?.length);

  return hasReadReplicas ? (
    <ReadReplicasGroup source={source} isInconsistent={isInconsistent} />
  ) : (
    <li>
      <div className={`${styles['dagCard']} ${styles['dagCardClickable']}`}>
        <div className={`${styles['dagHeader']} ${styles['flexMiddle']}`}>
          <Flex className="font-bold" align="center" gap="1">
            <FaDatabase aria-hidden="true" />
            {sourceName}
          </Flex>
        </div>
        <div className={styles['dagBody']}>
          <p className={`${styles['sm']} ${styles['muted']}`}>
            <HealthStatus
              status={isInconsistent ? 'Inconsistent' : 'Healthy'}
              showStatusText
            />
          </p>
        </div>
      </div>
      {source.kind === 'postgres' && <DBEvents source={source} />}
    </li>
  );
};

export default DatasourceItem;
