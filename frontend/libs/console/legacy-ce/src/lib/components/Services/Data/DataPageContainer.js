import { Link } from 'react-router';
import globals from '../../../Globals';
import { Analytics, REDACT_EVERYTHING } from '../../../features/Analytics';

import clsx from 'clsx';
import { CLI_CONSOLE_MODE } from '../../../constants';
import { Sidebar as NewSidebar } from '../../../features/DataSidebar/Sidebar';
import { SIDEBAR_ID } from '../../../features/DataSidebar/constants';
import {
  availableFeatureFlagIds,
  useIsFeatureFlagEnabled,
} from '../../../features/FeatureFlags';
import { useMetadata } from '../../../features/hasura-metadata-api';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import styles from '../../Common/TableCommon/Table.module.scss';
import DataSubSidebar from './DataSubSidebar';

const DataPageContainer = ({ children, location }) => {
  const currentLocation = location.pathname;

  const { data: areSourcesPresent = false } = useMetadata(
    m => !!m.metadata.sources.length
  );

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

  const legacySidebarContent = (
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
        {areSourcesPresent && (
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
            <li
              role="presentation"
              className={
                currentLocation.includes('/model-count-summary')
                  ? styles.active
                  : ''
              }
            >
              <Link
                className={styles.linkBorder}
                to={`/data/model-count-summary`}
                data-test="model-count-summary"
              >
                Model Summary
              </Link>
            </li>
          </>
        )}
        {migrationTab}
      </ul>
    </Analytics>
  );

  const helmet = 'Data | Hasura';

  const { enabled: performanceModeEnabled } = useIsFeatureFlagEnabled(
    availableFeatureFlagIds.performanceMode
  );

  const leftContainer = (
    <div
      id={SIDEBAR_ID}
      className={clsx(
        !performanceModeEnabled &&
          `${styles.pageSidebar} ${styles.padd_remove}`,
        performanceModeEnabled && `h-[calc(100vh-56px)]`
      )}
    >
      {performanceModeEnabled ? <NewSidebar /> : legacySidebarContent}
    </div>
  );

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
