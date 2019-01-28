import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
import PageContainer from './PageContainer/PageContainer';

const appPrefix = '/events';

const EventHeader = ({
  schema,
  currentSchema,
  children,
  location,
  dispatch,
}) => {
  const styles = require('../Data/TableCommon/Table.scss');
  const currentLocation = location.pathname;
  return (
    <div>
      <Helmet title={'Events | Hasura'} />
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
                      to={appPrefix + '/manage'}
                    >
                      Manage
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

const eventHeaderConnector = connect => connect(mapStateToProps)(EventHeader);

export default eventHeaderConnector;
