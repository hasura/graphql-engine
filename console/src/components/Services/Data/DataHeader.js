import React from 'react';
import { Link } from 'react-router';
import _push from './push';
import Helmet from 'react-helmet';
import PageContainer from './PageContainer/PageContainer';
import globals from '../../../Globals';

import {
  loadSchema,
  loadUntrackedSchema,
  loadUntrackedRelations,
  UPDATE_CURRENT_SCHEMA,
} from './DataActions';

const sectionPrefix = '/data';

const DataHeader = ({
  schema,
  currentSchema,
  schemaList,
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
        <Link
          className={styles.sidebarMigration}
          to={sectionPrefix + '/migrations'}
        >
          Migrations
        </Link>
      </li>
    );
  }

  const handleSchemaChange = e => {
    const updatedSchema = e.target.value;
    dispatch(_push(`/schema/${updatedSchema}`));
    Promise.all([
      dispatch({ type: UPDATE_CURRENT_SCHEMA, currentSchema: updatedSchema }),
      dispatch(loadSchema()),
      dispatch(loadUntrackedSchema()),
      dispatch(loadUntrackedRelations()),
    ]);
  };
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
                      to={sectionPrefix + '/schema'}
                    >
                      Schema:
                    </Link>
                    <select
                      onChange={handleSchemaChange}
                      className={styles.changeSchema + ' form-control'}
                    >
                      {schemaList.map(s => (
                        <option
                          key={s.schema_name}
                          selected={s.schema_name === currentSchema}
                        >
                          {s.schema_name}
                        </option>
                      ))}
                    </select>
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
                <Link
                  className={styles.wd100}
                  to={sectionPrefix + '/sql'}
                  data-test="sql-link"
                >
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
