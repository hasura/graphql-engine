import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import { getReactHelmetTitle } from '../../../../Common/utils/reactUtils';
import BreadCrumb from '../../../../Common/Layout/BreadCrumb/BreadCrumb';
import {
  getETModifyRoute,
  getETProcessedEventsRoute,
  getDataEventsLandingRoute,
} from '../../../../Common/utils/routesUtils';
import { EVENTS_SERVICE_HEADING } from '../../constants';

const TableHeader = ({ triggerName, tabName, count, readOnlyMode }) => {
  const styles = require('./EventTable.scss');
  let showCount = '';
  if (!(count === null || count === undefined)) {
    showCount = '(' + count + ')';
  }
  let activeTab;
  if (tabName === 'processed') {
    activeTab = 'Processed';
  } else if (tabName === 'pending') {
    activeTab = 'Pending';
  } else if (tabName === 'modify') {
    activeTab = 'Modify';
  } else if (tabName === 'logs') {
    activeTab = 'Invocation Logs';
  }

  const getBreadCrumbs = () => {
    return [
      {
        title: 'Events',
        url: '',
      },
      {
        title: 'Data Triggers',
        url: getDataEventsLandingRoute(),
      },
      {
        title: triggerName,
        url: getETProcessedEventsRoute(triggerName),
      },
      {
        title: activeTab,
        url: null,
      },
    ];
  };

  return (
    <div>
      <Helmet
        title={getReactHelmetTitle(
          `${activeTab} - ${triggerName}`,
          EVENTS_SERVICE_HEADING
        )}
      />
      <div className={styles.subHeader}>
        <BreadCrumb breadCrumbs={getBreadCrumbs()} />
        <h2 className={styles.heading_text}>{triggerName}</h2>
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            {!readOnlyMode && (
              <li
                role="presentation"
                className={tabName === 'modify' ? styles.active : ''}
              >
                <Link
                  to={getETModifyRoute({ name: triggerName })}
                  data-test="trigger-modify"
                >
                  Modify
                </Link>
              </li>
            )}
            <li
              role="presentation"
              className={tabName === 'pending' ? styles.active : ''}
            >
              <Link
                to={`/events/data/${triggerName}/pending`}
                data-test="trigger-pending-events"
              >
                Pending Events {tabName === 'pending' ? showCount : null}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'processed' ? styles.active : ''}
            >
              <Link
                to={`/events/data/${triggerName}/processed`}
                data-test="trigger-processed-events"
              >
                Processed Events {tabName === 'processed' ? showCount : null}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'logs' ? styles.active : ''}
            >
              <Link
                to={`/events/data/${triggerName}/logs`}
                data-test="trigger-invocation-logs"
              >
                Invocation Logs
              </Link>
            </li>
          </ul>
        </div>
        <div className="clearfix" />
      </div>
    </div>
  );
};
export default TableHeader;
