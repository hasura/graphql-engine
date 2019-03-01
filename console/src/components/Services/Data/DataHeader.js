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
  fetchFunctionInit,
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
          currentLocation.includes('data/migrations') ? styles.active : ''
        }
      >
        <Link className={styles.linkBorder} to={sectionPrefix + '/migrations'}>
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
      dispatch(fetchFunctionInit()),
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
                  currentLocation.includes('data/schema') ? styles.active : ''
                }
              >
                <Link
                  className={styles.linkBorder}
                  to={sectionPrefix + '/schema/' + currentSchema}
                >
                  <div className={styles.schemaWrapper}>
                    <div
                      className={styles.schemaSidebarSection}
                      data-test="schema"
                    >
                      Schema:
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
                </Link>
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
                  currentLocation.includes('data/sql') ? styles.active : ''
                }
              >
                <Link
                  className={styles.linkBorder}
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
