import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';

const TableHeader = ({ triggerName, tabName, count }) => {
  const styles = require('./EventTable.scss');
  let capitalised = tabName;
  capitalised = capitalised[0].toUpperCase() + capitalised.slice(1);
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
        url: '/events',
      },
      {
        title: 'Manage',
        url: '/events/manage',
      },
      {
        title: 'Triggers',
        url: '/events/manage/triggers/',
      },
      {
        title: triggerName,
        url: '/events/manage/triggers/' + triggerName + '/processed',
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
        title={capitalised + ' - ' + triggerName + ' - Event Triggers | Hasura'}
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
                to={'/events/manage/triggers/' + triggerName + '/processed'}
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
                to={'/events/manage/triggers/' + triggerName + '/pending'}
                data-test="trigger-pending-events"
              >
                Pending Events {tabName === 'pending' ? showCount : null}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'running' ? styles.active : 'hide'}
            >
              <Link
                to={'/events/manage/triggers/' + triggerName + '/running'}
                data-test="trigger-running-events"
              >
                Running {tabName === 'running' ? showCount : null}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'logs' ? styles.active : ''}
              data-test="trigger-logs"
            >
              <Link to={'/events/manage/triggers/' + triggerName + '/logs'}>
                Invocation Logs
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'modify' ? styles.active : ''}
              data-test="trigger-modify"
            >
              <Link to={'/events/manage/triggers/' + triggerName + '/modify'}>
                Modify
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
