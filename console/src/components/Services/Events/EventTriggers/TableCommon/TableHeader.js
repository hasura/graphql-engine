import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import { capitalize } from '../../../../Common/utils/jsUtils';
import BreadCrumb from '../../../../Common/Layout/BreadCrumb/BreadCrumb';
import {
  getETModifyRoute,
  getETInvocationLogsRoute,
  getETPendingEventsRoute,
  getETProcessedEventsRoute,
  getDataEventsLandingRoute,
} from '../../../../Common/utils/routesUtils';

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
    activeTab = 'Logs';
  }

  const getBreadCrumbs = () => {
    return [
      {
        title: 'Events',
        url: '',
      },
      {
        title: 'Manage',
        url: getDataEventsLandingRoute(),
      },
      {
        title: 'Data Events',
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
        title={
          capitalize(tabName) +
          ' - ' +
          triggerName +
          ' - Event Triggers | Hasura'
        }
      />
      <div className={styles.subHeader}>
        <BreadCrumb breadCrumbs={getBreadCrumbs()} />
        <h2 className={styles.heading_text}>{triggerName}</h2>
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            <li
              role="presentation"
              className={tabName === 'processed' ? styles.active : ''}
            >
              <Link
                to={getETProcessedEventsRoute(triggerName)}
                data-test="trigger-processed-events"
              >
                Processed Events {tabName === 'processed' ? showCount : null}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'pending' ? styles.active : ''}
            >
              <Link
                to={getETPendingEventsRoute(triggerName)}
                data-test="trigger-pending-events"
              >
                Pending Events {tabName === 'pending' ? showCount : null}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'logs' ? styles.active : ''}
              data-test="trigger-logs"
            >
              <Link
                to={getETInvocationLogsRoute(triggerName)}
                data-test="trigger-invocation-logs"
              >
                Invocation Logs
              </Link>
            </li>
            {!readOnlyMode && (
              <li
                role="presentation"
                className={tabName === 'modify' ? styles.active : ''}
                data-test="trigger-modify"
              >
                <Link
                  to={getETModifyRoute(triggerName)}
                  data-test="trigger-invocation-logs"
                >
                  Modify
                </Link>
              </li>
            )}
          </ul>
        </div>
        <div className="clearfix" />
      </div>
    </div>
  );
};
export default TableHeader;
