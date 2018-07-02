import React from 'react';
import { Link } from 'react-router';
import Helmet from 'react-helmet';
// import { push } from 'react-router-redux';

import PageContainer from './PageContainer/PageContainer';

import { appPrefix } from './push';
/*
import {
  UPDATE_CURRENT_SCHEMA,
  loadSchema,
  loadUntrackedSchema,
} from './DataActions';
*/
import globals from '../../../Globals';

const DataHeader = ({
  schema,
  // schemaList,
  currentSchema,
  children,
  location,
  dispatch,
}) => {
  const styles = require('./TableCommon/Table.scss');
  const currentLocation = location.pathname;
  /*
  const handleSchemaChange = e => {
    const updatedSchema = e.target.value;
    dispatch(push(globals.urlPrefix + '/data/schema/' + updatedSchema));
    Promise.all([
      dispatch({ type: UPDATE_CURRENT_SCHEMA, currentSchema: updatedSchema }),
      dispatch(loadSchema()),
      dispatch(loadUntrackedSchema()),
    ]);
  };
  */
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
                      Schema
                    </Link>
                  </div>
                  <div className={styles.schemaSidebarSection}>
                    {/* disable dropdown selection for now
                    <select
                      onChange={handleSchemaChange}
                      className={'form-control'}
                    >
                      {schemaList.map(s => {
                        if (s.schema_name === currentSchema) {
                          return (
                            <option key={s.schema_name} selected="selected">
                              {s.schema_name}
                            </option>
                          );
                        }
                        return (
                          <option key={s.schema_name}>{s.schema_name}</option>
                        );
                      })}
                    </select>
                    */}
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
                <Link to={appPrefix + '/sql'}>SQL</Link>
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
