import React, { useEffect } from 'react';
import { Link } from 'react-router';
import { Analytics, REDACT_EVERYTHING } from '../../../features/Analytics';
import globals from '../../../Globals';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import DataSubSidebar from './DataSubSidebar';
import { CLI_CONSOLE_MODE } from '../../../constants';
import styles from '../../Common/TableCommon/Table.module.scss';
import { isFeatureSupported } from '../../../dataSources';
import { fetchPostgresVersion } from '../../Main/Actions';
// import { useEnvironmentState } from '../../../features/ConnectDBRedesign/hooks';

const DataPageContainer = ({
  children,
  location,
  dispatch,
  currentDataSource,
}) => {
  // const { consoleType } = useEnvironmentState();
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
    <Analytics name="DataPageContainerSidebar" {...REDACT_EVERYTHING}>
      <ul className="bootstrap-jail">
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
          <Link
            data-testid="Data Manager"
            className={styles.linkBorder}
            to={`/data/manage`}
          >
            Data Manager
          </Link>

          <DataSubSidebar />
        </li>
        {currentDataSource && (
          <>
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
            <li
              role="presentation"
              className={
                currentLocation.includes('/native-queries') ||
                currentLocation.includes('/logical-models')
                  ? styles.active
                  : ''
              }
            >
              <Link
                className={styles.linkBorder}
                to={`/data/native-queries`}
                data-test="native-queries"
              >
                Native Queries
              </Link>
            </li>
          </>
        )}
        {migrationTab}
      </ul>
    </Analytics>
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
