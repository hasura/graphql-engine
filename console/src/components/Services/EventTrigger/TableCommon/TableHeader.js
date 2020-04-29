import React from 'react';
import { Link as RouterLink } from 'react-router';
import Helmet from 'react-helmet';

import { capitalize } from '../../../Common/utils/jsUtils';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import { Heading } from '../../../UIKit/atoms';
import styles from './EventTable.scss';

const TableHeader = ({ triggerName, tabName, count, readOnlyMode }) => {
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
        title={
          capitalize(tabName) +
          ' - ' +
          triggerName +
          ' - Event Triggers | Hasura'
        }
      />
      <div className={styles.subHeader}>
        <BreadCrumb breadCrumbs={getBreadCrumbs()} />
        <Heading as="h2" fontSize="18px" pb="20px">
          {triggerName}
        </Heading>
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            <li
              role="presentation"
              className={tabName === 'processed' ? styles.active : ''}
            >
              <RouterLink
                to={'/events/manage/triggers/' + triggerName + '/processed'}
                data-test="trigger-processed-events"
              >
                Processed Events {tabName === 'processed' ? showCount : null}
              </RouterLink>
            </li>
            <li
              role="presentation"
              className={tabName === 'pending' ? styles.active : ''}
            >
              <RouterLink
                to={'/events/manage/triggers/' + triggerName + '/pending'}
                data-test="trigger-pending-events"
              >
                Pending Events {tabName === 'pending' ? showCount : null}
              </RouterLink>
            </li>
            <li
              role="presentation"
              className={tabName === 'running' ? styles.active : 'hide'}
            >
              <RouterLink
                to={'/events/manage/triggers/' + triggerName + '/running'}
                data-test="trigger-running-events"
              >
                Running {tabName === 'running' ? showCount : null}
              </RouterLink>
            </li>
            <li
              role="presentation"
              className={tabName === 'logs' ? styles.active : ''}
              data-test="trigger-logs"
            >
              <RouterLink
                to={'/events/manage/triggers/' + triggerName + '/logs'}
              >
                Invocation Logs
              </RouterLink>
            </li>
            {!readOnlyMode && (
              <li
                role="presentation"
                className={tabName === 'modify' ? styles.active : ''}
                data-test="trigger-modify"
              >
                <RouterLink
                  to={'/events/manage/triggers/' + triggerName + '/modify'}
                >
                  Modify
                </RouterLink>
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
