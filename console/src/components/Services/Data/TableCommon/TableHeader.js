import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';

const TableHeader = ({
  tableName,
  tabName,
  count,
  migrationMode,
  currentSchema,
}) => {
  const styles = require('./Table.scss');
  let capitalised = tabName;
  capitalised = capitalised[0].toUpperCase() + capitalised.slice(1);
  let showCount = '';
  if (!(count === null || count === undefined)) {
    showCount = '(' + count + ')';
  }
  let activeTab;
  if (tabName === 'view') {
    activeTab = 'Browse Rows';
  } else if (tabName === 'insert') {
    activeTab = 'Insert Row';
  } else if (tabName === 'modify') {
    activeTab = 'Modify';
  } else if (tabName === 'relationships') {
    activeTab = 'Relationships';
  } else if (tabName === 'permissions') {
    activeTab = 'Permissions';
  }
  return (
    <div>
      <Helmet title={capitalised + ' - ' + tableName + ' - Data | Hasura'} />
      <div className={styles.subHeader}>
        <div className={styles.dataBreadCrumb}>
          You are here: <Link to={'/data/schema/' + currentSchema}>Data</Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" />{' '}
          <Link to={'/data/schema/' + currentSchema}>Schema</Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" />{' '}
          <Link to={'/data/schema/' + currentSchema}>{currentSchema}</Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" />{' '}
          <Link
            to={
              '/data/schema/' +
              currentSchema +
              '/tables/' +
              tableName +
              '/browse'
            }
          >
            {tableName}
          </Link>{' '}
          <i className="fa fa-angle-right" aria-hidden="true" /> {activeTab}
        </div>
        <h2 className={styles.heading_text}>{tableName}</h2>
        <div className={styles.nav}>
          <ul className="nav nav-pills">
            <li
              role="presentation"
              className={tabName === 'view' ? styles.active : ''}
            >
              <Link
                to={
                  '/data/schema/' +
                  currentSchema +
                  '/tables/' +
                  tableName +
                  '/browse'
                }
                data-test="table-browse-rows"
              >
                Browse Rows {showCount}
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'insert' ? styles.active : ''}
            >
              <Link
                to={
                  '/data/schema/' +
                  currentSchema +
                  '/tables/' +
                  tableName +
                  '/insert'
                }
                data-test="table-insert-rows"
              >
                Insert Row
              </Link>
            </li>
            {migrationMode ? (
              <li
                role="presentation"
                className={tabName === 'modify' ? styles.active : ''}
              >
                <Link
                  to={
                    '/data/schema/' +
                    currentSchema +
                    '/tables/' +
                    tableName +
                    '/modify'
                  }
                  data-test="table-modify"
                >
                  Modify
                </Link>
              </li>
            ) : null}
            <li
              role="presentation"
              className={tabName === 'relationships' ? styles.active : ''}
            >
              <Link
                to={
                  '/data/schema/' +
                  currentSchema +
                  '/tables/' +
                  tableName +
                  '/relationships'
                }
                data-test="table-relationships"
              >
                Relationships
              </Link>
            </li>
            <li
              role="presentation"
              className={tabName === 'permissions' ? styles.active : ''}
            >
              <Link
                to={
                  '/data/schema/' +
                  currentSchema +
                  '/tables/' +
                  tableName +
                  '/permissions'
                }
                data-test="table-permissions"
              >
                Permissions
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
