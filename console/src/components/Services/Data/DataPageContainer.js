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
import { push } from 'react-router-redux';
import { fetchPostgresVersion } from '../../Main/Actions';

const DataPageContainer = ({
  children,
  location,
  dispatch,
  dataSources,
  currentDataSource,
  currentSchema,
}) => {
  useEffect(() => {
    if (!currentDataSource && dataSources.length) {
      dispatch({
        type: UPDATE_CURRENT_DATA_SOURCE,
        source: dataSources[0].name,
      });
    }
  }, [currentDataSource, dataSources, dispatch]);

  const [loading, setLoading] = useState(false);

  useEffect(() => {
    if (currentDataSource) {
      dispatch(fetchPostgresVersion);
    }
  }, [dispatch, currentDataSource]);

  const { setDriver } = useDataSource();

  const handleDatabaseChange = value => {
    if (value === currentDataSource) return;
    setLoading(true);
    setDriver(currentDriver);
    dispatch({
      type: UPDATE_CURRENT_DATA_SOURCE,
      source: value,
    });
    dispatch(push(`/data/${value}/schema/`));
    dispatch(fetchDataInit()).then(() => {
      setLoading(false);
    });
  };

  const handleSchemaChange = value => {
    if (value === currentSchema) {
      dispatch(push(`/data/${currentDataSource}/schema/${value}`));
      return;
    }

    setLoading(true);
    dispatch(updateCurrentSchema(value, currentDataSource)).then(() => {
      dispatch(push(`/data/${currentDataSource}/schema/${value}`));
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
            to={`/data/${currentDataSource}/sql`}
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
