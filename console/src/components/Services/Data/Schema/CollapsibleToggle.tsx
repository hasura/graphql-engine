import React, { useState } from 'react';

import { driverToLabel } from '../../../../dataSources';
import { DataSource } from '../../../../metadata/types';
import styles from './styles.scss';

type CollapsibleToggleProps = {
  dataSource: DataSource;
  dbVersion: string;
};

const CollapsibleToggle: React.FC<CollapsibleToggleProps> = ({
  dataSource,
  dbVersion,
}) => {
  const [isOpen, setIsOpen] = useState(false);

  const toggleHandler = () => setIsOpen(prev => !prev);

  return (
    <div className={styles.collapsibleWrapper}>
      <div
        className={styles.collapsibleToggle}
        onClick={toggleHandler}
        role="button"
        tabIndex={0}
      >
        <span className={styles.collapsibleIndicatorWrapper}>
          <i
            className={`fa fa-chevron-down ${styles.collapsibleIndicator} ${
              isOpen && styles.collapsibleIndicatorOpen
            }`}
          />{' '}
          {isOpen ? <span>Less Info</span> : <span>More Info</span>}
        </span>

        <span className={`${styles.titleWrapper} ${styles.add_mar_bottom_mid}`}>
          <span className={styles.dataSource_label}>{dataSource.name}</span>(
          {driverToLabel[dataSource.driver]})
          {!!dataSource?.read_replicas?.length && (
            <span className={styles.replica_badge}>
              {dataSource.read_replicas.length} Replicas
            </span>
          )}
        </span>
      </div>
      <br />

      {isOpen ? (
        <>
          {dbVersion ? (
            <div
              className={`${styles.add_mar_bottom_mid} ${styles.display_flex}`}
            >
              <div className={styles.dbversion_label}>Database</div>
              <div>{dbVersion}</div>
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.max_connections ? (
            <div className={styles.add_mar_bottom_mid}>
              <span className={styles.dataSource_label}>Max Connections</span>
              {dataSource.connection_pool_settings?.max_connections}
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.idle_timeout ? (
            <div className={styles.add_mar_bottom_mid}>
              <span className={styles.dataSource_label}>Idle Timeout</span>
              {dataSource.connection_pool_settings?.idle_timeout}
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.retries ? (
            <div className={styles.add_mar_bottom_mid}>
              <span className={styles.dataSource_label}>Retries</span>
              {dataSource.connection_pool_settings?.retries}
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.pool_timeout ? (
            <div className={styles.add_mar_bottom_mid}>
              <span className={styles.dataSource_label}>Pool Timeout</span>
              {dataSource.connection_pool_settings?.pool_timeout}
            </div>
          ) : null}
          {dataSource.connection_pool_settings?.connection_lifetime ? (
            <div>
              <span className={styles.dataSource_label}>
                Connection Lifetime
              </span>
              {dataSource.connection_pool_settings?.connection_lifetime}
            </div>
          ) : null}
        </>
      ) : (
        ''
      )}
    </div>
  );
};

export default CollapsibleToggle;
