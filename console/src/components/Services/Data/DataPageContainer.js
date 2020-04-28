import React from 'react';
import { Link as RouterLink } from 'react-router';
import globals from '../../../Globals';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import DataSubSidebar from './DataSubSidebar';
import GqlCompatibilityWarning from '../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';

import { updateCurrentSchema } from './DataActions';
import { NotFoundError } from '../../Error/PageNotFound';
import { CLI_CONSOLE_MODE } from '../../../constants';
import { getSchemaBaseRoute } from '../../Common/utils/routesUtils';

const DataPageContainer = ({
  currentSchema,
  schemaList,
  children,
  location,
  dispatch,
}) => {
  const styles = require('../../Common/TableCommon/Table.scss');

  if (!schemaList.map(s => s.schema_name).includes(currentSchema)) {
    dispatch(updateCurrentSchema('public', false));

    // throw a 404 exception
    throw new NotFoundError();
  }

  const currentLocation = location.pathname;

  let migrationTab = null;
  if (globals.consoleMode === CLI_CONSOLE_MODE) {
    migrationTab = (
      <li
        role="presentation"
        className={
          currentLocation.includes('data/migrations') ? styles.active : ''
        }
      >
        <RouterLink className={styles.linkBorder} to={'/data/migrations'}>
          Migrations
        </RouterLink>
      </li>
    );
  }

  const handleSchemaChange = e => {
    dispatch(updateCurrentSchema(e.target.value));
  };

  const getSchemaOptions = () => {
    return schemaList.map(s => (
      <option key={s.schema_name} value={s.schema_name}>
        {s.schema_name}
      </option>
    ));
  };

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={currentLocation.includes('data/schema') ? styles.active : ''}
      >
        <RouterLink
          className={styles.linkBorder}
          to={getSchemaBaseRoute(currentSchema)}
        >
          <div className={styles.schemaWrapper}>
            <div className={styles.schemaSidebarSection} data-test="schema">
              Schema:
              <select
                onChange={handleSchemaChange}
                value={currentSchema}
                className={styles.changeSchema + ' form-control'}
              >
                {getSchemaOptions()}
              </select>
              <GqlCompatibilityWarning identifier={currentSchema} />
            </div>
          </div>
        </RouterLink>
        <DataSubSidebar location={location} />
      </li>
      <li
        role="presentation"
        className={currentLocation.includes('data/sql') ? styles.active : ''}
      >
        <RouterLink
          className={styles.linkBorder}
          to={'/data/sql'}
          data-test="sql-link"
        >
          SQL
        </RouterLink>
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
    schemaList: state.tables.schemaList,
    currentSchema: state.tables.currentSchema,
  };
};

const dataPageConnector = connect =>
  connect(mapStateToProps)(DataPageContainer);

export default dataPageConnector;
