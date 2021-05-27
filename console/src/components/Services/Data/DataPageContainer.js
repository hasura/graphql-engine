import React, { useEffect } from 'react';
import { Link } from 'react-router';
import globals from '../../../Globals';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import DataSubSidebar from './DataSubSidebar';
import { CLI_CONSOLE_MODE } from '../../../constants';
import styles from '../../Common/TableCommon/Table.scss';
import { isFeatureSupported } from '../../../dataSources';
import { fetchPostgresVersion } from '../../Main/Actions';

const DataPageContainer = ({
  children,
  location,
  dispatch,
  currentDataSource,
}) => {
  useEffect(() => {
    // TODO: handle for different drivers
    if (
      currentDataSource &&
      isFeatureSupported('driver.fetchVersion.enabled')
    ) {
      dispatch(fetchPostgresVersion);
    }
  }, [dispatch, currentDataSource]);

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

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={
          currentLocation.match(
            /(\/)?data((\/manage)|(\/(\w+)\/)|(\/(\w|%)+\/schema?(\w+)))/
          )
            ? styles.active
            : ''
        }
      >
        <Link className={styles.linkBorder} to={`/data/manage`}>
          Data Manager
        </Link>

        <DataSubSidebar />
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
    currentDataSource: state.tables.currentDataSource,
  };
};

const dataPageConnector = connect =>
  connect(mapStateToProps)(DataPageContainer);

export default dataPageConnector;
