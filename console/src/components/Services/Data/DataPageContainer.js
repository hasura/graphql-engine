import React, { useEffect } from 'react';
import { Link } from 'react-router';
import globals from '../../../Globals';

import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import DataSubSidebar from './DataSubSidebar';
import { CLI_CONSOLE_MODE } from '../../../constants';
import styles from '../../Common/Common.scss';
import { isFeatureSupported } from '../../../dataSources';
import { fetchPostgresVersion } from '../../Main/Actions';
import clsx from 'clsx'

const DataManager = ({ location }) => {
  return (
    <li
      className={clsx(
        location.pathname.match(
          /(\/)?data((\/manage)|(\/(\w+)\/)|(\/(\w|%)+\/schema?(\w+)))/
        ) && styles.active,
        ''
      )}
    >
      <Link
        className={clsx(styles.linkBorder, styles.moduleLink)}
        to="/data/manage"
      >
        Data Manager
      </Link>
      <div className='overflow-y-auto p-4'>
        <DataSubSidebar />
      </div>
    </li>
  )
}

const SQL = ({ ds, location }) => {
  if (ds == null) {
    return null
  }

  return (
    <li
      className={clsx(location.pathname.includes('/sql') && styles.active)}
    >
      <Link
        className={clsx(
          styles.linkBorder,
          styles.moduleLink
        )}
        to="/data/sql"
        data-test="sql-link"
      >
        SQL
      </Link>
    </li>
  )
}

const Migrations = ({location}) => {
  if (globals.consoleMode === CLI_CONSOLE_MODE) {
    return null
  }

  return (
    <li
      className={clsx(location.pathname.includes('data/migrations') && styles.active)}
    >
      <Link
        className={clsx(
          styles.linkBorder,
          styles.moduleLink
        )}
        to={'/data/migrations'}
      >
        Migrations
      </Link>
    </li>
  )
}

const Sidebar = ({ location, ds }) => {
  return (
    <ul>
      <DataManager location={location} />
      <SQL location={location} ds={ds} />
      <Migrations location={location} />
    </ul>
  )
}

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

  return (
    <PageContainer
      title={'Data | Hasura'}
      leftContainer={<Sidebar location={location} ds={currentDataSource} />}
    >
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
