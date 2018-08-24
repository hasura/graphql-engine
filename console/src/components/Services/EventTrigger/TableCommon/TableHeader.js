import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';

const TableHeader = ({ triggerName, tabName, count, currentSchema }) => {
  const styles = require('./Table.scss');
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
  } else if (tabName === 'settings') {
    activeTab = 'Settings';
  }
  return (
    <div>
      <Helmet
        title={capitalised + ' - ' + triggerName + ' - Event Triggers | Hasura'}
      />
      <div className={styles.subHeader}>
        <div className={styles.dataBreadCrumb}>
          You are here: <Link to={'/events'}>Events</Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" />{' '}
          <Link to={'/events/manage'}>Manage</Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" />{' '}
          <Link to={'/events/manage/triggers' + currentSchema}>Triggers</Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" />{' '}
          <Link to={'/events/manage/triggers/' + triggerName + '/processed'}>
            {triggerName}
          </Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" /> {activeTab}
        </div>
        <h2 className={styles.heading_text}>{triggerName}</h2>
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            <li
              role="presentation"
              className={tabName === 'processed' ? styles.active : ''}
            >
              <Link
                to={'/events/manage/triggers/' + triggerName + '/processed'}
                data-test="table-browse-rows"
              >
                Processed {tabName === 'processed' ? showCount : null}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'pending' ? styles.active : ''}
            >
              <Link
                to={'/events/manage/triggers/' + triggerName + '/pending'}
                data-test="table-insert-rows"
              >
                Pending {tabName === 'pending' ? showCount : null}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'settings' ? styles.active : ''}
            >
              <Link to={'/events/manage/triggers/' + triggerName + '/settings'}>
                Settings
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
