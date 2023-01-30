import React from 'react';
import { FaPlug, FaSearch } from 'react-icons/fa';

import styles from '../MetricsV1.module.scss';
import HealthStatus from './HealthStatus';

const RemoteSchemaItem = ({ source, inconsistentObjects }) => {
  const { name, definition } = source;
  const sourceName = name || definition || '';
  const isInconsistent =
    inconsistentObjects.find(
      i => i.type === 'remote_schema' && i?.definition?.name === sourceName
    ) !== undefined;

  return (
    <li>
      <div className={`${styles.dagCard} database`}>
        <div className={`${styles.dagHeader} ${styles.flexMiddle} `}>
          <p className={`${styles.strong} ${styles.mr_xs}`}>
            <FaPlug className={`${styles.mr_xxs}`} aria-hidden="true" />
            {sourceName}
          </p>
          <FaSearch
            className={`${styles.muted} ${styles.search}`}
            aria-hidden="true"
          />
        </div>
        <div className={styles.dagBody}>
          <p className={`${styles.sm} ${styles.muted}`}>
            <HealthStatus
              status={isInconsistent ? 'Inconsistent' : 'Healthy'}
              showStatusText
            />
          </p>
        </div>
      </div>
    </li>
  );
};

export default RemoteSchemaItem;
