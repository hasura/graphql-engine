import React, { useEffect, useState } from 'react';
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
import styles from '../../Common/TableCommon/Table.scss';
import { currentDriver, useDataSource } from '../../../dataSources';
import { getDataSources } from '../../../metadata/selector';
import { push } from 'react-router-redux';
import { fetchPostgresVersion } from '../../Main/Actions';

const DataPageContainer = ({
  currentSchema,
  schemaList,
  children,
  location,
  dispatch,
  dataSources,
  currentDataSource,
}) => {
  useEffect(() => {
    if (!currentDataSource && dataSources.length) {
      dispatch({
        type: UPDATE_CURRENT_DATA_SOURCE,
        source: dataSources[0].name,
      });
    }
  }, [currentDataSource, dataSources, dispatch]);

  useEffect(() => {
    if (currentDataSource) {
      dispatch(fetchPostgresVersion);
    }
  }, [dispatch, currentDataSource]);

  const { setDriver } = useDataSource();
  const [loadingSchemas, setLoadingSchemas] = useState(false);
  const onDatabaseChange = e => {
    const value = e.target.value;
    let newName;
    let newDriver;
    try {
      [newName, newDriver] = JSON.parse(value);
    } catch {
      return;
    }
    setDriver(newDriver);
    dispatch({
      type: UPDATE_CURRENT_DATA_SOURCE,
      source: newName,
    });
    dispatch(push(`/data/${newName}/schema/`));
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
    dispatch(updateCurrentSchema(e.target.value, currentDataSource));
  };

  const getSchemaOptions = () => {
    return schemaList.map(s => (
      <option key={s} value={s}>
        {s}
      </option>
    ));
  };

  const sidebarContent = (
    <ul>
      <li
        role="presentation"
        className={
          currentLocation.match(/(\/)?data\/(\w|%)+\/schema?(\w+)/)
            ? styles.active
            : ''
        }
      >
        <section className={`${styles.linkBorder} ${styles.dbSelect}`}>
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
                className={`${styles.changeSchema} form-control`}
                value={JSON.stringify([currentDataSource, currentDriver])}
              >
                {dataSources.map(s => (
                  <option
                    key={s.name}
                    value={JSON.stringify([s.name, s.driver])}
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
                value={currentDataSource ? currentSchema : ''}
                className={styles.changeSchema + ' form-control'}
              >
                <option value="" />
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
        </section>
        <DataSubSidebar location={location} />
      </li>
      {currentDataSource && (
        <li
          role="presentation"
          className={currentLocation.includes('/sql') ? styles.active : ''}
        >
          <Link
            className={styles.linkBorder}
            to={`/data/${currentDataSource}/sql`}
            data-test="sql-link"
          >
            SQL
          </Link>
        </li>
      )}
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
