import React from 'react';
import HealthStatus from './HealthStatus';

import styles from '../MetricsV1.module.scss';
import DBEvents from './DBEvents';
import { FaClone, FaDatabase, FaHome, FaSearch } from 'react-icons/fa';

const isActive = (src, selectedSrc, ix) => {
  if (
    ix > 0 &&
    selectedSrc?.isReadReplica &&
    selectedSrc?.name === `${src.name} Replica ${ix}`
  ) {
    return true;
  } else if (
    ix === 0 && // master DB will always be at 0th position
    !selectedSrc?.isReadReplica &&
    selectedSrc?.name === src.name
  ) {
    return true;
  }
  return false;
};

const ReadReplicasGroup = ({
  source,
  isInconsistent,
  setSelectedSource,
  selectedSource,
}) => {
  const replicas = source?.configuration?.read_replicas || [];
  const setSelected = (i, ix) => e => {
    e.stopPropagation();
    if (ix === 0) {
      setSelectedSource({
        ...i,
        hasReadReplica: true,
      });
    } else {
      setSelectedSource({
        ...i,
        isReadReplica: true,
        hasReadReplica: true,
        name: `${source.name} Replica ${ix}`,
        sourceName: source.name,
        ix,
      });
    }
  };

  return (
    <li>
      <div className={`${styles.dagContainer}`}>
        <div className={styles.dagHeader}>
          <p className={styles.strong}>
            <FaClone className="mr-xxs" aria-hidden="true" /> Read Replica Group
          </p>
        </div>

        <div className={styles.dagBodyGroup}>
          {[source, ...replicas].map((i, ix) => (
            <div
              key={i?.name || `${source?.name}_Replica_${ix}`}
              className={`${styles.mb_xs} ${styles.dagCard} ${
                styles.dagCardClickable
              } ${isActive(source, selectedSource, ix) && styles.active}`}
              onClick={setSelected(i, ix)}
            >
              <div className={`${styles.dagHeader} ${styles.flexMiddle}`}>
                <p className={`${styles.strong} ${styles.mr_xs}`}>
                  <FaDatabase className={styles.mr_xxs} aria-hidden="true" />
                  {ix === 0 ? (
                    <FaHome className={styles.mr_xxs} aria-hidden="true" />
                  ) : (
                    <FaClone className={styles.mr_xxs} aria-hidden="true" />
                  )}

                  {i?.name || `Replica ${ix}`}
                </p>
                <FaSearch
                  className={`${styles.muted} ${styles.search}`}
                  aria-hidden="true"
                />
              </div>
              <div className={styles.dagBody}>
                <HealthStatus
                  status={isInconsistent ? 'Inconsistent' : 'Healthy'}
                  showStatusText
                />
              </div>
            </div>
          ))}
        </div>
      </div>
      <DBEvents source={source} />
    </li>
  );
};

export default ReadReplicasGroup;
