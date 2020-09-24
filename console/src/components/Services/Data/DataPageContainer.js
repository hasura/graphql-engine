import React, { useState } from 'react';
import { Link } from 'react-router';
import globals from '../../../Globals';

import LeftContainer from '../../Common/Layout/LeftContainer/LeftContainer';
import PageContainer from '../../Common/Layout/PageContainer/PageContainer';
import DataSubSidebar from './DataSubSidebar';
import GqlCompatibilityWarning from '../../Common/GqlCompatibilityWarning/GqlCompatibilityWarning';

import {
  updateCurrentSchema,
  UPDATE_CURRENT_DATA_SOURCE,
  fetchDataInit,
} from './DataActions';
import { CLI_CONSOLE_MODE } from '../../../constants';
import { getSchemaBaseRoute } from '../../Common/utils/routesUtils';
import styles from '../../Common/TableCommon/Table.scss';
import { useDataSource } from '../../../dataSources';
import { getDataSources } from '../../../metadata/selector';
import { push } from 'react-router-redux';

const DataPageContainer = ({
  currentSchema,
  schemaList,
  children,
  location,
  dispatch,
  dataSources,
  currentDataSource,
}) => {
  const { setDriver } = useDataSource();
  const [loadingSchemas, setLoadingSchemas] = useState(false);

  const onDatabaseChange = e => {
    const value = e.target.value;
    let newName;
    let newDriver;
    try {
      [newName, newDriver] = JSON.parse(value);
    } catch (err) {
      return;
    }
    setDriver(newDriver);
    dispatch({
      type: UPDATE_CURRENT_DATA_SOURCE,
      source: newName,
    });
    dispatch(push('/data/schema/'));
    setLoadingSchemas(true);
    dispatch(fetchDataInit()).then(() => {
      setLoadingSchemas(false);
    });
  };

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
          // to={getSchemaBaseRoute(currentSchema)} // todo
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
                className={styles.changeSchema + ' form-control'}
              >
                {dataSources.map(s => (
                  <option
                    key={s.name}
                    value={JSON.stringify([s.name, s.driver])}
                    selected={s.name === currentDataSource}
                  >
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
                {!loadingSchemas && getSchemaOptions()}
              </select>
              {currentSchema && (
                <GqlCompatibilityWarning
                  identifier={currentSchema}
                  className={styles.add_mar_left_mid}
                />
              )}
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
    currentDataSource: state.tables.currentDataSource,
  };
};

const dataPageConnector = connect =>
  connect(mapStateToProps)(DataPageContainer);

export default dataPageConnector;
