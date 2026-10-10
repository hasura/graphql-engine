import { FaPlug } from 'react-icons/fa';

import styles from '../MetricsV1.module.scss';
import HealthStatus from './HealthStatus';
import { Flex } from '@radix-ui/themes';

const RemoteSchemaItem = ({ source, inconsistentObjects }) => {
  const { name, definition } = source;
  const sourceName = name || definition || '';
  const isInconsistent =
    inconsistentObjects.find(
      (i) => i.type === 'remote_schema' && i?.definition?.name === sourceName,
    ) !== undefined;

  return (
    <li>
      <div className={`${styles.dagCard} database`}>
        <div className={`${styles.dagHeader} ${styles.flexMiddle} `}>
          <Flex align="center" gap="1" className="font-bold">
            <FaPlug className={`${styles.mr_xxs}`} aria-hidden="true" />
            {sourceName}
          </Flex>
        </div>
        <div className={styles.dagBody}>
          <HealthStatus
            status={isInconsistent ? 'Inconsistent' : 'Healthy'}
            showStatusText
          />
        </div>
      </div>
    </li>
  );
};

export default RemoteSchemaItem;
