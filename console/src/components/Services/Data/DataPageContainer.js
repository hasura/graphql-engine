import React, { useState } from 'react';
import { Link } from 'react-router';
import globals from '../../../Globals';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import DataSubSidebar from './DataSubSidebar';
import GqlCompatibilityWarning from '../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';

import { updateCurrentSchema, fetchSchemaList } from './DataActions';
import { NotFoundError } from '../../Error/PageNotFound';
import { CLI_CONSOLE_MODE } from '../../../constants';
import { getSchemaBaseRoute } from '../../Common/utils/routesUtils';
import styles from '../../Common/TableCommon/Table.scss';
import { useDataSource } from '../../../dataSources';
import { getDataSources } from '../../../metadata/selector';

const DataPageContainer = ({
  currentSchema,
  schemaList,
  children,
  location,
  dispatch,
  metadata,
  dataSources,
}) => {
  const { driver, setDriver } = useDataSource();
  const [loadingSchemas, setLoadingSchemas] = useState(false);

  const onDatabaseChange = newDb => {
    console.log({
      newDb: newDb.target.value /** Users DB */,
      metadata,
      setDriver,
    });
    setDriver(/**whatever[newDb] */);
    setLoadingSchemas(true);
    dispatch(fetchSchemaList()).then(() => {
      setLoadingSchemas(false);
    });
  };

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
        <Link className={styles.linkBorder} to={'/data/migrations'}>
          Migrations
        </Link>
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
        <Link
          className={styles.linkBorder}
          to={getSchemaBaseRoute(currentSchema)}
        >
          <div className={styles.schemaWrapper}>
            <div
              className={styles.schemaSidebarSection}
              style={{
                marginBottom: '20px',
              }}
            >
              <label style={{ width: '70px' }}>Database:</label>
              <select
                onChange={onDatabaseChange}
                value={null /** todo */}
                className={styles.changeSchema + ' form-control'}
              >
                {dataSources.map(s => (
                  <option key={s.name} value={s.name}>
                    {s.name} ({s.driver})
                  </option>
                ))}
              </select>
            </div>
            <div className={styles.schemaSidebarSection} data-test="schema">
              <label style={{ width: '70px' }}>Schema:</label>
              <select
                onChange={handleSchemaChange}
                value={currentSchema}
                className={styles.changeSchema + ' form-control'}
              >
                {getSchemaOptions()}
              </select>
              <GqlCompatibilityWarning
                identifier={currentSchema}
                className={styles.add_mar_left_mid}
              />
            </div>
          </div>
        </Link>
        <DataSubSidebar location={location} />
      </li>
      <li
        role="presentation"
        className={currentLocation.includes('data/sql') ? styles.active : ''}
      >
        <Link
          className={styles.linkBorder}
          to={'/data/sql'}
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
    schemaList: state.tables.schemaList,
    currentSchema: state.tables.currentSchema,
    metadata: state.metadata.metadataObject,
    dataSources: getDataSources(state),
    currentDataSource: 'myDb (postgres)', // todo
  };
};

const dataPageConnector = connect =>
  connect(mapStateToProps)(DataPageContainer);

export default dataPageConnector;
