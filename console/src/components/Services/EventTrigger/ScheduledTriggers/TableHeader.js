import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import BreadCrumb from '../../../Common/Layout/BreadCrumb/BreadCrumb';
import Button from '../../../Common/Button/Button';
import { push } from 'react-router-redux';
import globals from '../../../../Globals';

const appPrefix = globals.urlPrefix + '/events';

const TableHeader = ({ dispatch, tabName, count }) => {
  const styles = require('../TableCommon/EventTable.scss');
  let capitalised = tabName;
  capitalised = capitalised[0].toUpperCase() + capitalised.slice(1);
  let showCount = '';
  if (!(count === null || count === undefined)) {
    showCount = '(' + count + ')';
  }
  let activeTab;
  if (tabName === 'scheduledTriggers') {
    activeTab = 'ScheduledTriggers';
  } else if (tabName === 'pastInvocations') {
    activeTab = 'Past Invocation';
  }

  const getBreadCrumbs = () => {
    return [
      {
        title: 'Events',
        url: '/events',
      },
      {
        title: 'Scheduled Triggers',
        url: '/events/scheduled-triggers',
      },
      {
        title: activeTab,
        url: null,
      },
    ];
  };

  const getAddBtn = () => {
    const handleClick = e => {
      e.preventDefault();

      dispatch(push(`${appPrefix}/scheduled-triggers/add`));
    };

    return (
      <Button
        data-test="data-create-trigger"
        color="yellow"
        size="sm"
        className={styles.add_mar_left}
        onClick={handleClick}
      >
        Create
      </Button>
    );
  };

  return (
    <div>
      <Helmet title={capitalised + ' - ' + ' - Scheduled Triggers | Hasura'} />
      <div className={styles.subHeader}>
        <BreadCrumb breadCrumbs={getBreadCrumbs()} />
        <div className={`${styles.display_flex} ${styles.padd_bottom}`}>
          <h2 className={`${styles.heading_text} ${styles.inline_block}`}>
            Manage Scheduled Triggers
          </h2>
          {getAddBtn()}
        </div>
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            <li
              role="presentation"
              className={tabName === 'scheduledTriggers' ? styles.active : ''}
            >
              <Link
                to={'/events/scheduled-triggers/view-scheduled-triggers'}
                data-test="tab-scheduled-triggers"
              >
                Scheduled Triggers{' '}
                {tabName === 'scheduledTriggers' ? showCount : null}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'pastInvocations' ? styles.active : ''}
            >
              <Link
                to={'/events/scheduled-triggers/view-past-invocations'}
                data-test="tab-past-invocations"
              >
                Past Invocations{' '}
                {tabName === 'pastInvocations' ? showCount : null}
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
