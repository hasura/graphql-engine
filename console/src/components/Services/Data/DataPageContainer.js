import React from 'react';
import { Link } from 'react-router';
import _push from './push';
import globals from '../../../Globals';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import DataSubSidebar from './DataSubSidebar';

import {
  loadSchema,
  loadUntrackedSchema,
  loadUntrackedRelations,
  UPDATE_CURRENT_SCHEMA,
  fetchFunctionInit,
} from './DataActions';

const sectionPrefix = '/data';

const DataPageContainer = ({
  schema,
  currentSchema,
  schemaList,
  children,
  location,
  dispatch,
}) => {
  const styles = require('../../Common/TableCommon/Table.scss');

  const currentLocation = location.pathname;

  let migrationTab = null;
  if (globals.consoleMode === 'cli') {
    migrationTab = (
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

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={currentLocation.includes('data/schema') ? styles.active : ''}
      >
        <Link
          className={styles.linkBorder}
          to={sectionPrefix + '/schema/' + currentSchema}
        >
          <div className={styles.schemaWrapper}>
            <div className={styles.schemaSidebarSection} data-test="schema">
              Schema:
              <select
                onChange={handleSchemaChange}
                value={currentSchema}
                className={styles.changeSchema + ' form-control'}
              >
                {schemaList.map(s => (
                  <option key={s.schema_name} value={s.schema_name}>
                    {s.schema_name}
                  </option>
                ))}
              </select>
            </div>
          </div>
        </Link>
        <DataSubSidebar
          location={location}
          schema={schema}
          currentSchema={currentSchema}
          dispatch={dispatch}
        />
      </li>
      <li
        role="presentation"
        className={currentLocation.includes('data/sql') ? styles.active : ''}
      >
        <Link
          className={styles.linkBorder}
          to={sectionPrefix + '/sql'}
          data-test="sql-link"
        >
          SQL
        </Link>
      </li>
      {migrationTab}
    </ul>
  );

  const helmet = 'Data | Hasura';

  const leftContainer = <LeftContainer>{sidebarContent}</LeftContainer>;

  return (
    <PageContainer helmet={helmet} leftContainer={leftContainer}>
      {children}
    </PageContainer>
  );
};

const mapStateToProps = state => {
  return {
    schema: state.tables.allSchemas,
    schemaList: state.tables.schemaList,
    currentSchema: state.tables.currentSchema,
  };
};

const dataPageConnector = connect =>
  connect(mapStateToProps)(DataPageContainer);

export default dataPageConnector;
