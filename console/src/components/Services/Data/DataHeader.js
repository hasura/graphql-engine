import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import PageContainer from './PageContainer/PageContainer';
import globals from '../../../Globals';

const appPrefix = '/data';

const DataHeader = ({
  schema,
  currentSchema,
  children,
  location,
  dispatch,
}) => {
  const styles = require('./TableCommon/Table.scss');
  const currentLocation = location.pathname;
  let migrationSection = null;
  if (globals.consoleMode === 'cli') {
    migrationSection = (
      <li
        role="presentation"
        className={
          currentLocation.indexOf('migrations') !== -1 ? styles.active : ''
        }
      >
        <Link to={appPrefix + '/migrations'}>Migrations</Link>
      </li>
    );
  }
  return (
    <div>
      <Helmet title={'Data | Hasura'} />
      <div className={styles.wd20 + ' ' + styles.align_left}>
        <div
          className={styles.pageSidebar + ' col-xs-12 ' + styles.padd_remove}
        >
          <div>
            <ul>
              <li
                role="presentation"
                className={
                  currentLocation.indexOf('schema') !== -1 ? styles.active : ''
                }
              >
                <div className={styles.schemaWrapper}>
                  <div
                    className={styles.schemaSidebarSection}
                    data-test="schema"
                  >
                    <Link
                      className={styles.schemaBorder}
                      to={appPrefix + '/schema'}
                    >
                      Schema - {currentSchema}
                    </Link>
                  </div>
                </div>
                <PageContainer
                  location={location}
                  schema={schema}
                  currentSchema={currentSchema}
                  dispatch={dispatch}
                />
              </li>
              <li
                role="presentation"
                className={
                  currentLocation.indexOf('sql') !== -1 ? styles.active : ''
                }
              >
                <Link to={appPrefix + '/sql'} data-test="sql-link">
                  SQL
                </Link>
              </li>
              {migrationSection}
            </ul>
          </div>
        </div>
      </div>
      <div className={styles.wd80}>{children}</div>
    </div>
  );
};

const mapStateToProps = state => {
  return {
    schema: state.tables.allSchemas,
    schemaList: state.tables.schemaList,
    currentSchema: state.tables.currentSchema,
  };
};

const dataHeaderConnector = connect => connect(mapStateToProps)(DataHeader);

export default dataHeaderConnector;
