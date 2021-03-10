import React, { useEffect, useState } from 'react';
import { Link } from 'react-router';
import globals from '../../../Globals';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import DataSubSidebar from './DataSubSidebar';
import {
  updateCurrentSchema,
  UPDATE_CURRENT_DATA_SOURCE,
  fetchDataInit,
} from './DataActions';
import { CLI_CONSOLE_MODE } from '../../../constants';
import styles from '../../Common/TableCommon/Table.scss';
import { currentDriver, useDataSource } from '../../../dataSources';
import { getDataSources } from '../../../metadata/selector';
import _push from './push';
import { fetchPostgresVersion } from '../../Main/Actions';
import { getSourceDriver } from './utils';

const DataPageContainer = ({
  children,
  location,
  dispatch,
  dataSources,
  currentDataSource,
  currentSchema,
}) => {
  const { setDriver } = useDataSource();

  // useEffect(() => {
  //   if (!currentDataSource && dataSources.length) {
  //     setDriver(dataSources[0].driver);
  //     dispatch({
  //       type: UPDATE_CURRENT_DATA_SOURCE,
  //       source: dataSources[0].name,
  //     });
  //   }
  // }, [currentDataSource, dataSources, dispatch]);

  const [loading, setLoading] = useState(false);

  useEffect(() => {
    // TODO: handle for different drivers
    if (currentDataSource && currentDriver === 'postgres') {
      dispatch(fetchPostgresVersion);
    }
  }, [dispatch, currentDataSource]);

  const handleDatabaseChange = newSourceName => {
    if (newSourceName === currentDataSource) {
      dispatch(_push(`/data/${newSourceName}/`));
      return;
    }
    setLoading(true);
    const driver = getSourceDriver(dataSources, newSourceName);
    dispatch({
      type: UPDATE_CURRENT_DATA_SOURCE,
      source: newSourceName,
    });
    setDriver(driver);
    dispatch(_push(`/data/${newSourceName}/`));
    dispatch(fetchDataInit()).finally(() => {
      setLoading(false);
    });
  };

  const handleSchemaChange = value => {
    if (value === currentSchema) {
      dispatch(_push(`/data/${currentDataSource}/schema/${value}`));
      return;
    }

    setLoading(true);
    dispatch(updateCurrentSchema(value, currentDataSource))
      .then(() => {
        dispatch(_push(`/data/${currentDataSource}/schema/${value}`));
      })
      .finally(() => {
        setLoading(false);
      });
  };

  const currentLocation = location.pathname;

  let migrationTab = null;
  if (globals.consoleMode === CLI_CONSOLE_MODE) {
    migrationTab = (
      <li
        role="presentation"
        className={
          currentLocation.includes('data/migrations') ? styles.active : ''
        }
      >
        <Link className={styles.linkBorder} to={'/data/migrations'}>
          Migrations
        </Link>
      </li>
    );
  }

  const loadStyle = {
    pointerEvents: 'none',
    cursor: 'progress',
  };

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={
          currentLocation.match(
            /(\/)?data((\/manage)|(\/(\w|%)+\/schema?(\w+)))/
          )
            ? styles.active
            : ''
        }
      >
        <Link className={styles.linkBorder} to={`/data/manage`}>
          Data Manager
        </Link>

        <div style={loading ? loadStyle : { pointerEvents: 'auto' }}>
          <DataSubSidebar
            onDatabaseChange={handleDatabaseChange}
            onSchemaChange={handleSchemaChange}
          />
        </div>
      </li>
      {currentDataSource && (
        <li
          role="presentation"
          className={currentLocation.includes('/sql') ? styles.active : ''}
        >
          <Link
            className={styles.linkBorder}
            to={`/data/sql`}
            data-test="sql-link"
          >
            SQL
          </Link>
        </li>
      )}
      {migrationTab}
    </ul>
  );

  const helmet = 'Data | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;

  return (
    <PageContainer helmet={helmet} leftContainer={leftContainer}>
      {children}
    </PageContainer>
  );
};

const mapStateToProps = state => {
  return {
    schemaList: state.tables.schemaList,
    currentSchema: state.tables.currentSchema,
    metadata: state.metadata.metadataObject,
    dataSources: getDataSources(state),
    currentDataSource: state.tables.currentDataSource,
  };
};

const dataPageConnector = connect =>
  connect(mapStateToProps)(DataPageContainer);

export default dataPageConnector;
