import React from 'react';
import DBEvents from './DBEvents';
import HealthStatus from './HealthStatus';
import ReadReplicasGroup from './ReadReplicasGroup';

import styles from '../MetricsV1.module.scss';
import { FaDatabase, FaSearch } from 'react-icons/fa';

const DatasourceItem = ({
  source,
  inconsistentObjects = [],
  selectedSource,
  setSelectedSource,
}) => {
  const { name, definition } = source;
  const sourceName = name || definition || '';
  const isInconsistent =
    inconsistentObjects.find(i => i?.definition === sourceName) !== undefined;
  const keyHandler = event => {
    if (event.key === 'Enter') return setSelectedSource(source);
  };
  const hasReadReplicas = source?.configuration?.read_replicas?.length > 0;
  const active = selectedSource?.name === source.name;

  return hasReadReplicas ? (
    <ReadReplicasGroup
      source={source}
      sourceName={sourceName}
      isInconsistent={isInconsistent}
      setSelectedSource={setSelectedSource}
      selectedSource={selectedSource}
    />
  ) : (
    <li>
      <div
        className={`${styles.dagCard} ${styles.dagCardClickable} ${
          active && styles.active
        }`}
        onClick={() => setSelectedSource(source)}
        onKeyPress={keyHandler}
        role="button"
        tabIndex={0}
      >
        <div className={`${styles.dagHeader} ${styles.flexMiddle}`}>
          <p className={`${styles.strong} ${styles.mr_xs}`}>
            <FaDatabase className={styles.mr_xxs} aria-hidden="true" />
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
      <DBEvents source={source} />
    </li>
  );
};

export default DatasourceItem;
