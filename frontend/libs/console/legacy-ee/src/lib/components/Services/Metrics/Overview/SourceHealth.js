import React, { useState } from 'react';
import { Col } from 'react-bootstrap';
import DataSourceItem from './DataSourceItem';
import RemoteSchemaItem from './RemoteSchemaItem';
import ActionsItem from './ActionsItem';
import DatabaseInfoCard from './DatabaseInfoCard';
import OverallHealthCard from './OverallHealthCard';

import styles from '../MetricsV1.module.scss';
import AccessDenied from '../../../AccessDenied/AccessDenied';
import { isAdmin } from '../utils';
import clsx from 'clsx';

const SourceHealth = ({
  inconsistentObjects,
  metadata: { sources = [], actions = [], remote_schemas = [] } = {},
  project,
}) => {
  const [selectedSource, setSelectedSource] = useState({});

  const _isAdmin = isAdmin(project.privileges);

  return (
    <div className={clsx(styles.sourceHealth, 'bootstrap-jail')}>
      <Col md={12} lg={_isAdmin ? 8 : 12} className={styles.no_pad}>
        <div>
          <p className={`${styles.strong} ${styles.padding_top_20}`}>
            Source Health
          </p>
          {_isAdmin ? (
            <ul
              className={`${styles.tree} ${styles.ul_pad_remove} ${styles.horizontal}`}
            >
              <li>
                <OverallHealthCard />
                <ul className={styles.ul_pad_remove}>
                  {sources &&
                    sources.map((source, ix) => (
                      <DataSourceItem
                        key={`DataSource_${
                          source?.name || Math.floor(Math.random() * ix * 1000)
                        }`}
                        source={source}
                        selectedSource={selectedSource}
                        setSelectedSource={setSelectedSource}
                        inconsistentObjects={inconsistentObjects}
                      />
                    ))}
                  {remote_schemas &&
                    remote_schemas.map((source, ix) => (
                      <RemoteSchemaItem
                        source={source}
                        inconsistentObjects={inconsistentObjects}
                        key={`RemoteSchema_${
                          source?.name || Math.floor(Math.random() * ix * 1000)
                        }`}
                      />
                    ))}
                  {Array.isArray(actions) && actions.length > 0 && (
                    <ActionsItem actions={actions} />
                  )}
                </ul>
              </li>
            </ul>
          ) : (
            <AccessDenied alignCenter={false} />
          )}
        </div>
      </Col>
      {_isAdmin && (
        <Col md={12} lg={4} className={styles.databaseInfoCardCol}>
          <DatabaseInfoCard
            selectedSource={selectedSource}
            projectId={project.id}
            key={selectedSource?.name}
          />
        </Col>
      )}
    </div>
  );
};

export default SourceHealth;
